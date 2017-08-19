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

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Tasty.Tmux where

import qualified Data.Text as T
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Data.Semigroup ((<>))
import Control.Concurrent
       (newEmptyMVar, putMVar, takeMVar)
import System.Timeout (timeout)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
       (register, release, resourceForkIO, runResourceT, ResourceT, ReleaseKey)
import Control.Exception (catch, IOException)
import System.IO (hPutStr, stderr)
import Control.Monad (void)

import System.Process (callProcess)
import System.Directory
       (getCurrentDirectory, removeFile, getTemporaryDirectory,
        removeDirectoryRecursive)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile)

import Network.Socket hiding (recv)
import Network.Socket
       (bind, socket, Family(..), SocketType(..), defaultProtocol, SockAddr(..))
import Network.Socket.ByteString (recv)
import Data.ByteString.Char8 (pack, ByteString)

testMakesHardcopy ::
  TestTree
testMakesHardcopy = goldenVsFile "does not crash" "test/data/test.golden" "/tmp/testoutput" (runResourceT $ tmuxSession steps "purebredtest")
  where steps = [ApplicationStep ["Enter"]]

data ApplicationStep = ApplicationStep [String]

tmuxSession :: [ApplicationStep] -> String -> ResourceT IO ()
tmuxSession xs sessionname = do
    systmp <- liftIO $ getCanonicalTemporaryDirectory
    testdir <- liftIO $ createTempDirectory systmp "purebredtest"
    rkey <- register (removeDirectoryRecursive testdir)
    mdir <-
        liftIO $
        do mdir <- prepareMaildir testdir
    tmuxRkey <- createTmuxSession sessionname
    liftIO $
        do runSteps xs
           snapshotState sessionname
    release tmuxRkey
    release rkey

runSteps :: [ApplicationStep] -> IO ()
runSteps steps = mapM_ (\(ApplicationStep xs) -> callProcess "tmux" (communicateSessionArgs ++ xs)) steps

snapshotState :: String -> IO ()
snapshotState sessionname = do
    callProcess "tmux" hardcopyArgs
    callProcess "tmux" (savebufferArgs ++ ["/tmp/testoutput"])
    where
      hardcopyArgs = ["capture-pane", "-b","purebredcapture","-t", sessionname]
      savebufferArgs = ["save-buffer", "-b", "purebredcapture"]

createTmuxSession :: String -> ResourceT IO ReleaseKey
createTmuxSession sessionname = do
    liftIO $
        callProcess
            "tmux"
            [ "new-session"
            , "-x"
            , "80"
            , "-y"
            , "24"
            , "-d"
            , "-s"
            , sessionname
            , "-n"
            , "purebred"]
    register (cleanUpTmuxSession sessionname)

cleanUpTmuxSession :: String -> IO ()
cleanUpTmuxSession sessionname = do
    catch
        (callProcess "tmux" ["kill-session", "-t", sessionname])
        (\e ->
              do let err = show (e :: IOException)
                 hPutStr stderr ("Exception when killing session: " ++ err)
                 pure ())

waitReady :: SockAddr -> ResourceT IO ()
waitReady addr = do
    liftIO $
        do soc <- socket AF_UNIX Datagram defaultProtocol
           bind soc addr
           d <- recv soc 4096
           if d /= applicationReadySignal
               then error "application did not start up in time"
               else close soc

applicationReadySignal :: ByteString
applicationReadySignal = pack "READY=1"

communicateSessionArgs :: [String]
communicateSessionArgs = words "send-keys -t purebredtest"

