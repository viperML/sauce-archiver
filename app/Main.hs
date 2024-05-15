{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Colog (
    LogAction,
    Message,
    Msg (..),
    WithLog,
    cmap,
    fmtMessage,
    formatWith,
    logError,
    logInfo,
    logTextStderr,
    logTextStdout,
    msgSeverity,
    msgText,
    richMessageAction,
    showSeverity,
    usingLoggerT, logWarning, logDebug, cfilter, Severity (Debug),
 )

import Data.Text (Text, pack)
import Prelude hiding (log)
import System.Environment (getEnvironment, getEnv)
import UnliftIO

import Cli

example1 :: WithLog env Message m => m ()
example1 = do
    logInfo "this is a demo log for message!"
    logWarning "xd"
    logDebug "xd"

logStdoutAction :: LogAction IO Message
logStdoutAction = cmap fmtMessage logTextStdout

fmtMessageWithoutSourceLoc :: Message -> Text
fmtMessageWithoutSourceLoc Msg{..} =
    showSeverity msgSeverity
        <> msgText


example2 :: (WithLog env Message m, MonadUnliftIO m) => m ()
example2 = do
    logInfo "xd"
    logDebug "goodbye"
    return ()




main :: IO ()
main = do
    let myFilter = cfilter (\Msg{..} -> msgSeverity > Debug)
    usingLoggerT (myFilter logStdoutAction) $ do
        concurrently_ example2 example2
    opts <- parsed
    return ()
