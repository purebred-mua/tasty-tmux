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
import Control.Monad (forM_)
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectory, copyFile, listDirectory)
import Data.Semigroup ((<>))
import GHC.MVar (MVar)
import Control.Concurrent (newEmptyMVar, forkIO, putMVar, takeMVar, killThread, threadDelay)
import Control.Exception (bracket)

import System.Process
       (shell, callProcess, readProcess, readCreateProcess, CreateProcess(..),
        CmdSpec(..), StdStream(..))
import System.Directory (getCurrentDirectory, removeFile)
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
    out <- prepareNotmuch cfg
    print out
    _ <- setUp testmdir
    callProcess "tmux" $ communicateSessionArgs ++ ["j", "j", "Enter"]
    out <- readProcess "tmux" hardcopyArgs []
    print out
    callProcess "tmux" $ savebufferArgs "/tmp/testoutput"
    teardown

waitReady :: MVar String -> IO ()
waitReady baton = do
    soc <- socket AF_UNIX Datagram defaultProtocol
    bind soc (SockAddrUnix "/tmp/purebred.socket")
    d <- recv soc 4096
    if d /= applicationReadySignal
        then error "application did not start up in time"
        else close soc >> removeFile "/tmp/purebred.socket" >> putMVar baton "ready"

applicationReadySignal :: ByteString
applicationReadySignal = pack "READY=1"

tmuxSessionArgs :: FilePath -> [String]
tmuxSessionArgs cfg =
    [ "new-session"
    , "-x"
    , "95"
    , "-y"
    , "56"
    , "-d"
    , "-s"
    , "purebredtest"
    , "-n"
    , "purebred"
    , "purebred"
    , "--database"
    , cfg]

communicateSessionArgs :: [String]
communicateSessionArgs = words "send-keys -t purebredtest"

hardcopyArgs :: [String]
hardcopyArgs = words "capture-pane -p -t purebredtest:purebred"

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

