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

import Data.Char (isAscii, isAlphaNum, chr)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Data.Functor (($>))
import Data.Semigroup ((<>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Exception (catch, IOException)
import System.IO (hPutStr, hPutStrLn, stderr, stdout)
import System.Environment (lookupEnv)
import Control.Monad (void, when)
import Data.Maybe (isJust)
import Data.List (intercalate, isInfixOf)
import qualified Data.ByteString.Lazy as LB
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT, ask, ReaderT)

import Control.Lens (Lens', view)
import System.Process.Typed
       (proc, runProcess_, withProcess_,
        waitExitCodeSTM, setStdout, getStdout, setStderr, useHandleOpen,
        byteStringOutput, setStdin, closed, ProcessConfig)
import System.Directory
       (getCurrentDirectory, removeDirectoryRecursive)
import Test.Tasty (TestTree, TestName, testGroup, withResource)
import Test.Tasty.HUnit (testCaseSteps, assertBool)
import Text.Regex.Posix ((=~))

-- | A condition to check for in the output of the program
data Condition
  = Literal String
  | Regex String
  deriving (Show)

assertSubstrInOutput :: String -> String -> ReaderT a IO ()
assertSubstrInOutput substr out = liftIO $ assertBool (substr <> " not found in\n\n" <> out) $ substr `isInfixOf` out

assertRegex :: String -> String -> ReaderT a IO ()
assertRegex regex out = liftIO $ assertBool
  (show regex <> " does not match out\n\n" <> out
    <> "\n\n raw:\n\n" <> show out)
  (out =~ regex)

sessionNamePrefix :: String
sessionNamePrefix = "purebredtest"

envSessionName :: Lens' Env String
envSessionName f (Env a b c) = fmap (\c' -> Env a b c') (f c)
{-# ANN envSessionName ("HLint: ignore Avoid lambda" :: String) #-}

-- | create a tmux session running in the background
-- Note: the width and height are the default values tmux uses, but I thought
-- it's better to be explicit.
setUpTmuxSession :: String -> IO ()
setUpTmuxSession sessionname =
    catch
        (runProcess_ $ proc
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
             , "purebred"])
        (\e ->
              do let err = show (e :: IOException)
                 hPutStrLn stderr ("\nException during setUp: " <> err)
                 pure ())

-- | Kills the whole session including pane and application
cleanUpTmuxSession :: String -> IO ()
cleanUpTmuxSession sessionname =
    catch
        (runProcess_ $ proc "tmux" ["kill-session", "-t", sessionname])
        (\e ->
              do let err = show (e :: IOException)
                 hPutStrLn stderr ("\nException when killing session: " <> err)
                 pure ())


-- | Run all application steps in a session defined by session name.
withTmuxSession
  :: TestName
  -> ((String -> IO ()) -> ReaderT Env IO ())
  -> Int  -- ^ session sequence number (will be appended to session name)
  -> TestTree
withTmuxSession tcname testfx i =
  withResource (setUp i tcname) tearDown $
      \env -> testCaseSteps tcname $ \stepfx -> env >>= runReaderT (testfx stepfx)

-- | Send keys into the program and wait for the condition to be
-- met, failing the test if the condition is not met after some
-- time.
sendKeys :: String -> Condition -> ReaderT Env IO String
sendKeys keys expect = do
    sessionName <- getSessionName
    runProcess_ $ proc "tmux" $ communicateSessionArgs sessionName keys False
    waitForCondition expect defaultCountdown

sendLiteralKeys :: String -> ReaderT Env IO String
sendLiteralKeys keys = do
    sessionName <- getSessionName
    runProcess_ $ proc "tmux" $ communicateSessionArgs sessionName keys True
    waitForString keys defaultCountdown

capture :: ReaderT Env IO String
capture = do
  sessionname <- getSessionName
  liftIO $ readProcessWithErrorOutput_ $ proc "tmux" ["capture-pane", "-e", "-p", "-t", sessionname]

getSessionName :: (Monad m) => ReaderT Env m String
getSessionName = view (envSessionName . ask)

holdOffTime :: Int
holdOffTime = 10 ^ (6 :: Int)

-- | wait for the application to render a new interface which we determine with
--   a given condition. We check up to @n@ times, waiting a short duration
--   between each check, and failing if the tries exhaust with the condition
--   not met.
waitForCondition :: Condition -> Int -> ReaderT Env IO String
waitForCondition cond n = do
  out <- capture >>= checkPane
  liftIO $ assertBool
    ( "Wait time exceeded. Condition not met: '" <> show cond
      <> "' last screen shot:\n\n " <> out <> "\n\n" <> " raw: " <> show out )
    (checkCondition cond out)
  pure out
  where
    checkPane :: String -> ReaderT Env IO String
    checkPane out
      | checkCondition cond out = pure out
      | n <= 0 = pure out
      | otherwise = do
          liftIO $ threadDelay holdOffTime
          waitForCondition cond (n - 1)

checkCondition :: Condition -> String -> Bool
checkCondition (Literal s) = (s `isInfixOf`)
checkCondition (Regex re) = (=~ re)

-- | Convenience version of 'waitForCondition' that checks for a
-- literal string.
--
waitForString :: String -> Int -> ReaderT Env IO String
waitForString = waitForCondition . Literal

defaultCountdown :: Int
defaultCountdown = 5

-- | Sets a shell environment variable
-- Note: The tmux program provides a command to set environment variables for
-- running sessions, yet they seem to be not inherited by the shell.
setEnvVarInSession :: String -> String -> ReaderT Env IO ()
setEnvVarInSession name value = do
  void $ sendLiteralKeys ("export " <> name <> "=" <> value)
  void $ sendKeys "Enter" (Literal name)

communicateSessionArgs
  :: String -- ^ session name
  -> String -- ^ keys
  -> Bool   -- ^ send the keys literally
  -> [String]
communicateSessionArgs sessionName keys asLiteral =
  ["send-keys", "-t", sessionName] <> ["-l" | asLiteral] <> [keys]


type AnsiAttrParam = String
type AnsiFGParam = String
type AnsiBGParam = String

-- | Generate a regex for an escape sequence setting the given
-- foreground and background parameters
--
-- tmux < 03d01eabb5c5227f56b6b44d04964c1328802628 (first released
-- in tmux-2.5) ran attributes, foreground colour and background
-- colour params separated by semicolons (foreground first).
--
-- After that commit, attributes, foreground colours and background
-- colours are written in separate escape sequences.  Therefore for
-- compatibility with different versions of tmux there are two
-- patterns to check.
--
buildAnsiRegex :: [AnsiAttrParam] -> [AnsiFGParam] -> [AnsiBGParam] -> String
buildAnsiRegex attrs fgs bgs =
  let
    withSemis = intercalate ";"
    wrap [] = ""
    wrap xs = "\ESC\\[" <> withSemis xs <> "m"
    tmux24 = wrap (attrs <> fgs <> bgs)
    tmux25 = wrap attrs <> wrap fgs <> wrap bgs
    choice "" "" = ""
    choice "" r = r
    choice l "" = l
    choice l r = "(" <> l <> "|" <> r <> ")"
  in
    choice tmux24 tmux25

-- | Interleaves stderr with stdout
-- Does not accept STDIN arguments, since we do not need to communicate with the
-- process
readProcessWithErrorOutput_ :: ProcessConfig stdinClosed stdout stderr -> IO String
readProcessWithErrorOutput_ pc = do
  (_, out) <- withProcess_ config $ \p -> atomically $ (,) <$> waitExitCodeSTM p <*> getStdout p
  pure $ T.unpack $ decodeLenient $ LB.toStrict out
  where
    config = setStdout byteStringOutput
             $ setStderr (useHandleOpen stdout)
             $ setStdin closed pc
    decodeLenient = T.decodeUtf8With T.lenientDecode
