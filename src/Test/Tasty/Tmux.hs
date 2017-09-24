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

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Tasty.Tmux where

import qualified Data.Text as T
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Data.Semigroup ((<>))
import Control.Concurrent (threadDelay)
import Control.Exception (catch, IOException)
import System.IO (hPutStr, stderr)
import System.Environment (lookupEnv)
import Control.Monad (void, when)
import Data.Maybe (isJust)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT, ask, ReaderT)

import Control.Lens (view, _3, _2)
import Data.List (isInfixOf)
import System.Process (callProcess, readProcess)
import System.Directory
       (getCurrentDirectory, removeDirectoryRecursive)
import Test.Tasty (TestTree, TestName, testGroup, withResource)
import Test.Tasty.HUnit (testCaseSteps, assertBool)
import Text.Regex.Posix ((=~))

type Env = (String, String, String)

assertSubstrInOutput :: String -> String -> ReaderT Env IO ()
assertSubstrInOutput substr out = liftIO $ assertBool (substr <> " not found in\n\n" <> out) $ substr `isInfixOf` out

assertRegex :: String -> String -> ReaderT Env IO ()
assertRegex regex out = liftIO $ assertBool (regex <> " does not match out\n\n" <> out) $ out =~ (regex :: String)

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
                 hPutStr stderr ("Exception when killing session: " <> err)
                 pure ())


-- | Run all application steps in a session defined by session name.
withTmuxSession :: TestName -> ((String -> IO ()) -> ReaderT Env IO ()) -> TestTree
withTmuxSession tcname testfx =
    withResource setUp tearDown $
      \env -> testCaseSteps tcname $ \stepfx -> env >>= runReaderT (testfx stepfx)

sendKeys :: String -> String -> ReaderT Env IO (String)
sendKeys keys expect = do
    liftIO $ callProcess "tmux" $ communicateSessionArgs keys False
    waitForString expect defaultCountdown

sendLiteralKeys :: String -> ReaderT Env IO (String)
sendLiteralKeys keys = do
    liftIO $ callProcess "tmux" $ communicateSessionArgs keys True
    waitForString keys defaultCountdown

capture :: ReaderT Env IO (String)
capture = do
  sessionname <- getSessionName
  liftIO $ readProcess "tmux" ["capture-pane", "-e", "-p", "-t", sessionname] []

getSessionName :: ReaderT Env IO (String)
getSessionName = view (_3 . ask)

holdOffTime :: Int
holdOffTime = 10^6

-- | wait for the application to render a new interface which we determine with
--   a given substring. If we exceed the number of tries return with the last
--   captured output, but indicate an error by setting the baton to 0
waitForString :: String -> Int -> ReaderT Env IO (String)
waitForString substr n = do
  out <- capture >>= checkPane
  liftIO $ assertBool ("Wait time exceeded. Expected: '"
                       <> substr
                       <> "' last screen shot:\n\n "
                       <> out) (substr `isInfixOf` out)
  pure out
  where
    checkPane :: String -> ReaderT Env IO String
    checkPane out
      | substr `isInfixOf` out = pure out
      | n <= 0 = pure out
      | otherwise = do
          liftIO $ threadDelay holdOffTime
          waitForString substr (n - 1)

defaultCountdown :: Int
defaultCountdown = 5

communicateSessionArgs :: String -> Bool -> [String]
communicateSessionArgs keys asLiteral =
    let base = words $ "send-keys -t " <> defaultSessionName
        postfix =
            if asLiteral
                then ["-l"]
                else []
    in base <> postfix <> [keys]
