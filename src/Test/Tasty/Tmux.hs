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
       (newEmptyMVar, putMVar, takeMVar, MVar, threadDelay)
import System.Timeout (timeout)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
       (register, release, runResourceT, ResourceT, ReleaseKey)
import Control.Exception (catch, IOException)
import System.IO (hPutStr, stderr)
import Control.Monad (void)

import Data.List (isInfixOf)
import System.Process (callProcess, readProcess)
import System.Directory
       (getCurrentDirectory, removeDirectoryRecursive)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import qualified Data.ByteString.Lazy as LBS

data ApplicationStep = ApplicationStep
    { asCommand :: String  -- ^ the actual commands to send
    , asAsLiteralKey :: Bool  -- ^ disables key name lookup and sends literal input
    , expected :: String  -- ^ wait until the terminal shows the expected string or timeout
    }

-- | Run all application steps in a session defined by session name.
--
-- In each session:
-- * we're preparing a temporary Maildir,
-- * configure and setup notmuch against it,
-- * create a tmux session with one panel,
-- * run all application steps against it and wait at each step if the application has rendered
-- * remove the temporary directory
--
-- Return the last captured pane state to compare it against the golden file.
tmuxSession :: [ApplicationStep] -> String -> ResourceT IO (LBS.ByteString)
tmuxSession xs sessionname = do
    systmp <- liftIO $ getCanonicalTemporaryDirectory
    testdir <- liftIO $ createTempDirectory systmp "purebredtest"
    mdir <-
        liftIO $
        do mdir <- prepareMaildir testdir
    tmuxRkey <- createTmuxSession sessionname
    liftIO $ startApplication sessionname mdir
    tout <-
        liftIO $
        do runSteps sessionname xs
           snapshotFinalState sessionname testdir
    release tmuxRkey
    -- only remove the tempdir if the whole session run was without problems,
    -- otherwise it'll help to debug issues
    liftIO $ removeDirectoryRecursive testdir
    pure tout

-- | run all steps, but timeout if the expected string can not be found
-- TODO: should we throw an exception?
runSteps :: String -> [ApplicationStep] -> IO ()
runSteps sessionname steps =
    void $ timeout hardStepTimeout $ mapM_ (performStep sessionname) steps

-- | maximum amount of time we allow a step to run until we fail it
-- 6 seconds should be plenty
hardStepTimeout :: Int
hardStepTimeout = 10 ^ 6 * 6

performStep :: String -> ApplicationStep -> IO ()
performStep sessionname (ApplicationStep xs asLiteral expect) = do
    callProcess "tmux" $ communicateSessionArgs xs asLiteral
    baton <- newEmptyMVar
    waitForString baton sessionname expect
    _ <- takeMVar baton
    pure ()

snapshotFinalState :: String -> FilePath -> IO (LBS.ByteString)
snapshotFinalState sessionname testdir = do
    let fp = testdir <> "/" <> sessionname <> "paneoutput.log"
    capturePane sessionname >>= writeFile fp
    LBS.readFile fp

capturePane :: String -> IO String
capturePane sessionname = readProcess "tmux" ["capture-pane", "-p", "-t", sessionname] []

holdOffTime :: Int
holdOffTime = 10^6

-- | wait for the application to render a new interface which we determine with
--   a given substring. If the expected substring is not in the captured pane,
--   wait a bit and try again.
waitForString :: MVar String -> String -> String -> IO ()
waitForString baton sessionname substr = do
    out <- readProcess "tmux" ["capture-pane", "-p", "-t", sessionname] []
    if substr `isInfixOf` out
        then putMVar baton "ready"
        else do
            threadDelay holdOffTime
            waitForString baton sessionname substr

-- | create a tmux session running in the background
-- Note: the width and height are the default values tmux uses, but I thought
-- it's better to be explicit.
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

communicateSessionArgs :: String -> Bool -> [String]
communicateSessionArgs keys asLiteral =
    let base = words "send-keys -t purebredtest"
        postfix =
            if asLiteral
                then ["-l"]
                else []
    in base ++ postfix ++ [keys]

