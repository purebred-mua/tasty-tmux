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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.Tmux where

import Data.Char (isAscii, isAlphaNum, chr)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Data.Semigroup ((<>))
import Control.Concurrent (threadDelay)
import Control.Exception (catch, IOException)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Environment (lookupEnv)
import Control.Monad (void, when)
import Data.Maybe (fromMaybe, isJust)
import Data.List (intercalate, isInfixOf)
import qualified Data.ByteString.Lazy as LB
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO, MonadReader, runReaderT, ReaderT)

import Control.Lens (Getter, Lens', to, view)
import System.Process.Typed
       (proc, runProcess_, readProcess_, readProcessInterleaved_,
        setEnv, ProcessConfig)
import Test.Tasty (TestTree, TestName, testGroup, withResource)
import Test.Tasty.HUnit (testCaseSteps, assertBool)
import Text.Regex.Posix ((=~))

-- | A condition to check for in the output of the program
data Condition
  = Unconditional
  | Literal String
  | Regex String
  deriving (Show)

-- | A test case that will be executed in a dedicated tmux session.
-- Parameterised over the "global" environment (c.f. the "session"
-- environment).
type TestCase a = IO a -> Int -> TestTree

-- | Run a series of tests in tmux sessions.
--
-- Tests are executed sequentially.  Each test case is executed in a
-- new tmux session.  The name of the session is derived from the
-- name of the test and prepended with the sequence number.
--
-- A session called "keepalive" is created before any test cases are
-- run, and killed after all the test cases have finished.  This
-- session ensures that the server remains alive, avoiding some race
-- conditions.
--
testTmux
  :: IO a
  -- ^ Set-up action.  Executed one time, after the keepalive
  -- session is created but before any test cases are executed.
  -> (a -> IO ())
  -- ^ Tear-down action.  Executed after all test cases have
  -- finished but before the keepalive session gets killed.
  -> [TestCase a]
  -> TestTree
testTmux pre post tests =
  withResource (frameworkPre *> pre) (\a -> post a *> frameworkPost) $ \env ->
    testGroup "user acceptance tests" $ zipWith ($ env) tests [0..]
  where
    keepaliveSessionName = "keepalive"
    frameworkPre = setUpTmuxSession keepaliveSessionName
    frameworkPost = cleanUpTmuxSession keepaliveSessionName

assertSubstrInOutput :: String -> String -> ReaderT a IO ()
assertSubstrInOutput substr out = liftIO $ assertBool (substr <> " not found in\n\n" <> out) $ substr `isInfixOf` out

assertRegex :: String -> String -> ReaderT a IO ()
assertRegex regex out = liftIO $ assertBool
  (show regex <> " does not match out\n\n" <> out
    <> "\n\n raw:\n\n" <> show out)
  (out =~ regex)

sessionNamePrefix :: String
sessionNamePrefix = "purebredtest"

type TmuxSession = String

class HasTmuxSession a where
  tmuxSession :: Lens' a TmuxSession

instance HasTmuxSession Env where
  tmuxSession = envSessionName

envSessionName :: Lens' Env String
envSessionName f (Env a b c d) = fmap (\d' -> Env a b c d') (f d)
{-# ANN envSessionName ("HLint: ignore Avoid lambda" :: String) #-}

-- | create a tmux session running in the background
-- Note: the width and height are the default values tmux uses, but I thought
-- it's better to be explicit.
--
-- Returns the session name (whatever the input was) for convenience.
setUpTmuxSession :: TmuxSession -> IO TmuxSession
setUpTmuxSession sessionname = sessionname <$
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
  :: (HasTmuxSession sessionEnv)
  => (globalEnv -> TmuxSession -> IO sessionEnv)
  -- ^ Set up session.  The tmux session is established before this
  -- action is run.  Takes the global environment and Tmux session
  -- and constructs a session environment value (which must make the
  -- 'TmuxSession' available via its 'HasTmuxSession' instance).
  -> (sessionEnv -> IO ())
  -- ^ Tear down the session.  The tmux session will be torn down
  -- /after/ this action.
  -> TestName
  -- ^ Name of the test (a string).
  -> ((String -> ReaderT sessionEnv IO ()) -> ReaderT sessionEnv IO a)
  -- ^ The main test function.  The argument is the "step" function
  -- which can be called with a description to label the steps of
  -- the test procedure.
  -> TestCase globalEnv
withTmuxSession pre post desc f getGEnv i =
  withResource
    (getGEnv >>= \gEnv -> frameworkPre >>= pre gEnv)
    (\env -> post env *> cleanUpTmuxSession (view tmuxSession env))
    $ \env -> testCaseSteps desc $
        \step -> env >>= runReaderT (void $ f (liftIO . step))
  where
    frameworkPre =
      let
        sessionName = intercalate "-" (sessionNamePrefix : show i : descWords)
        descWords = words $ filter (\c -> isAscii c && (isAlphaNum c || c == ' ')) desc
      in
        setUpTmuxSession sessionName

-- | Send keys into the program and wait for the condition to be
-- met, failing the test if the condition is not met after some
-- time.
sendKeys
  :: (HasTmuxSession a, MonadReader a m, MonadIO m)
  => String -> Condition -> m String
sendKeys keys expect = do
    tmuxSendKeys InterpretKeys keys
    waitForCondition expect defaultCountdown initialBackoffMicroseconds

sendLiteralKeys
  :: (HasTmuxSession a, MonadReader a m, MonadIO m)
  => String -> m String
sendLiteralKeys keys = do
    tmuxSendKeys LiteralKeys keys
    waitForString keys defaultCountdown

capture :: (HasTmuxSession a, MonadReader a m, MonadIO m) => m String
capture = T.unpack . decodeLenient . LB.toStrict
  <$> (tmuxSessionProc "capture-pane"
    [ "-e"  -- include escape sequences
    , "-p"  -- send output to stdout
    , "-J"  -- join wrapped lines and preserve trailing whitespace
    ]
  >>= liftIO . readProcessInterleaved_)
  where
    decodeLenient = T.decodeUtf8With T.lenientDecode

initialBackoffMicroseconds :: Int
initialBackoffMicroseconds = 20 * 10 ^ (3 :: Int)

-- | wait for the application to render a new interface which we determine with
--   a given condition. We wait a short duration and increase the wait time
--   exponentially until the count down reaches 0. We fail if until then the
--   condition is not met.
waitForCondition
  :: (HasTmuxSession a, MonadReader a m, MonadIO m)
  => Condition
  -> Int  -- ^ count down value
  -> Int  -- ^ milliseconds to back off
  -> m String
waitForCondition cond n backOff = do
  out <- capture >>= checkPane
  liftIO $ assertBool
    ( "Wait time exceeded. Condition not met: '" <> show cond
      <> "' last screen shot:\n\n " <> out <> "\n\n" <> " raw: " <> show out )
    (checkCondition cond out)
  pure out
  where
    checkPane out
      | checkCondition cond out = pure out
      | n <= 0 = pure out
      | otherwise = do
          liftIO $ threadDelay backOff
          waitForCondition cond (n - 1) (backOff * 4)

checkCondition :: Condition -> String -> Bool
checkCondition Unconditional = const True
checkCondition (Literal s) = (s `isInfixOf`)
checkCondition (Regex re) = (=~ re)

-- | Convenience version of 'waitForCondition' that checks for a
-- literal string.
--
waitForString
  :: (HasTmuxSession a, MonadReader a m, MonadIO m)
  => String -> Int -> m String
waitForString substr n = waitForCondition (Literal substr) n initialBackoffMicroseconds

defaultCountdown :: Int
defaultCountdown = 5

-- | Sets a shell environment variable
-- Note: The tmux program provides a command to set environment variables for
-- running sessions, yet they seem to be not inherited by the shell.
setEnvVarInSession
  :: (HasTmuxSession a, MonadReader a m, MonadIO m)
  => String -> String -> m ()
setEnvVarInSession name value = do
  void $ sendLiteralKeys ("export " <> name <> "=" <> value)
  void $ sendKeys "Enter" (Literal name)

-- | Whether to tell tmux to treat keys literally or interpret
-- sequences like "Enter" or "C-x".
--
data TmuxKeysMode = LiteralKeys | InterpretKeys
  deriving (Eq)

-- | Run a tmux command via 'runProcess_'.  The session name is read
-- from the 'MonadReader' environment
--
tmuxSendKeys
  :: (HasTmuxSession a, MonadReader a m, MonadIO m)
  => TmuxKeysMode -> String -> m ()
tmuxSendKeys mode keys = tmuxSendKeysProc mode keys >>= runProcess_

-- | Construct the 'ProcessConfig' for a tmux command.  The session
-- name is read from the 'MonadReader' environment.
--
tmuxSendKeysProc
  :: (HasTmuxSession a, MonadReader a m)
  => TmuxKeysMode -> String -> m (ProcessConfig () () ())
tmuxSendKeysProc mode keys = tmuxSessionProc "send-keys" (["-l" | mode == LiteralKeys] <> [keys])

-- | Create a 'ProcessConfig' for a tmux command, taking the session
-- name from the 'MonadReader' environment.
--
tmuxSessionProc
  :: (HasTmuxSession a, MonadReader a m)
  => String -> [String] -> m (ProcessConfig () () ())
tmuxSessionProc cmd args = do
  sessionName <- view tmuxSession
  pure $ proc "tmux" (cmd : "-t" : sessionName : args)



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
