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
import Control.Exception (catch, IOException)
import System.IO (hPutStr, stderr)
import Control.Monad (void)

import Data.List (isInfixOf)
import System.Process (callProcess, readProcess)
import System.Directory
       (getCurrentDirectory, removeDirectoryRecursive)
import Test.Tasty (TestTree, testGroup, withResource, mkTimeout, localOption)
import Test.Tasty.HUnit (testCaseSteps, assertBool, Assertion)

-- | maximum amount of time we allow a step to run until we fail it
-- 6 seconds should be plenty
testTimeout :: Integer
testTimeout = 10 ^ 6 * 8


data ApplicationStep = ApplicationStep
    { asKeys :: String  -- ^ the actual commands to send
    , asDescription :: String  -- ^ step definition
    , asAsLiteralKey :: Bool  -- ^ disables key name lookup and sends literal input
    , asExpected :: String  -- ^ wait until the terminal shows the expected string or timeout
    , asAssertInOutput :: String -> String -> Assertion  -- ^ assert this against the snapshot
    }

assertSubstrInOutput :: String -> String -> Assertion
assertSubstrInOutput out substr = assertBool "in out" $ substr `isInfixOf` out

defaultSessionName :: String
defaultSessionName = "purebredtest"

-- | create a tmux session running in the background
-- Note: the width and height are the default values tmux uses, but I thought
-- it's better to be explicit.
setUpTmuxSession :: String -> IO ()
setUpTmuxSession sessionname = do
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

-- | Kills the whole session including pane and application
cleanUpTmuxSession :: String -> IO ()
cleanUpTmuxSession sessionname = do
    catch
        (callProcess "tmux" ["kill-session", "-t", sessionname])
        (\e ->
              do let err = show (e :: IOException)
                 hPutStr stderr ("Exception when killing session: " ++ err)
                 pure ())


-- | Run all application steps in a session defined by session name.
tmuxSession :: IO String -> String -> [ApplicationStep] -> TestTree
tmuxSession _ tcname xs = testCaseSteps tcname $ \step -> runSteps step xs

runSteps :: (String -> IO ()) -> [ApplicationStep] -> IO ()
runSteps stepfx steps =
    mapM_
        (\a ->
              do stepfx (asDescription a)
                 out <- performStep "purebredtest" a
                 ((asAssertInOutput a) out (asExpected a)))
        steps

performStep :: String -> ApplicationStep -> IO (String)
performStep sessionname (ApplicationStep keys _ asLiteral expect _) = do
    callProcess "tmux" $ communicateSessionArgs keys asLiteral
    baton <- newEmptyMVar
    out <- waitForString baton sessionname expect
    _ <- takeMVar baton
    pure out

holdOffTime :: Int
holdOffTime = 10^6

-- | wait for the application to render a new interface which we determine with
--   a given substring. If the expected substring is not in the captured pane,
--   wait a bit and try again.
waitForString :: MVar String -> String -> String -> IO (String)
waitForString baton sessionname substr = do
    out <- readProcess "tmux" ["capture-pane", "-e", "-p", "-t", sessionname] []
    if substr `isInfixOf` out
        then putMVar baton "ready" >> pure out
        else do
            threadDelay holdOffTime
            waitForString baton sessionname substr

communicateSessionArgs :: String -> Bool -> [String]
communicateSessionArgs keys asLiteral =
    let base = words $ "send-keys -t " ++ defaultSessionName
        postfix =
            if asLiteral
                then ["-l"]
                else []
    in base ++ postfix ++ [keys]
