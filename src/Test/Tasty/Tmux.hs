-- This file is part of tasty-tmux
-- Copyright (C) 2017-2019 Róman Joost and Fraser Tweedale
--
-- tasty-tmux is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings #-}
module Test.Tasty.Tmux where

import Data.List (isInfixOf)
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import Data.Semigroup ((<>))
import GHC.MVar (MVar)
import Control.Concurrent
       (newEmptyMVar, forkIO, putMVar, takeMVar)
import System.Timeout (timeout)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
       (register, resourceForkIO, runResourceT, ResourceT)

import System.Process (callProcess, readProcess)
import System.Directory (getCurrentDirectory, removeFile, getTemporaryDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile)

import Network.Socket hiding (recv)
import Network.Socket
       (bind, socket, Family(..), SocketType(..), defaultProtocol, SockAddr(..))
import Network.Socket.ByteString (recv)
import Data.ByteString.Char8 (pack, ByteString)

testMakesHardcopy ::
  TestTree
testMakesHardcopy = goldenVsFile "does not crash" "test/data/test.golden" "/tmp/testoutput" smokeTest

smokeTest :: IO ()
smokeTest = do
  withSystemTempDirectory "purebredtest" $ \fp -> do
    testmdir <- prepareMaildir fp
    cfg <- prepareNotmuchCfg fp testmdir
    _ <- prepareNotmuch cfg
    callProcess "tmux" tmuxSessionArgs
    callProcess "tmux" (communicateSessionArgs ++ ["-l", "purebred --database " <> testmdir])
    callProcess "tmux" (communicateSessionArgs ++ ["Enter"])
    _ <- setUp
    callProcess "tmux" $ communicateSessionArgs ++ ["j", "j", "Enter"]
    readProcess "tmux" hardcopyArgs [] >>= print
    readProcess "tmux" (savebufferArgs "/tmp/testoutput") [] >>= print
    teardown

waitReady :: ResourceT IO ()
waitReady = do
    addr <- purebredSocketAddr
    liftIO $
        do soc <- socket AF_UNIX Datagram defaultProtocol
           bind soc addr
           d <- recv soc 4096
           if d /= applicationReadySignal
               then error "application did not start up in time"
               else close soc

applicationReadySignal :: ByteString
applicationReadySignal = pack "READY=1"

tmuxSessionArgs :: [String]
tmuxSessionArgs =
    [ "new-session"
    , "-x"
    , "80"
    , "-y"
    , "24"
    , "-d"
    , "-s"
    , "purebredtest"
    , "-n"
    , "purebred"
    ]

communicateSessionArgs :: [String]
communicateSessionArgs = words "send-keys -t purebredtest"

hardcopyArgs :: [String]
hardcopyArgs = words "capture-pane -p -b purebredcapture -t purebredtest:purebred"

savebufferArgs :: FilePath -> [String]
savebufferArgs hardcopypath =
    ["save-buffer", "-b", "purebredcapture", hardcopypath]

checkSessionStarted :: IO ()
checkSessionStarted = do
    out <- readProcess "tmux" ["list-sessions"] []
    print out
    if "purebredtest" `isInfixOf` out
        then pure ()
        else error "no session created"

teardown :: IO ()
teardown = do
  -- quit the application
  callProcess "tmux" $ communicateSessionArgs ++ (words "Esc Enter")
  -- XXX make sure it's dead
  callProcess "tmux" $ words "unlink-window -k -t purebredtest"

