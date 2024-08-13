{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr, LoggingT (runLoggingT), MonadLogger, defaultOutput, logDebugN, runStdoutLoggingT, fromLogStr)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import System.IO (Handle, stdout)
import qualified Data.ByteString.Char8 as S8

data Config = Config
    {
    }
    deriving (Show)

newtype App a = App
    { unApp :: ReaderT Config (LoggingT IO) a
    }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader Config)

outputFor ::
    Loc ->
    LogSource ->
    LogLevel ->
    LogStr ->
    IO ()
outputFor loc source level s = S8.putStrLn $ fromLogStr s

runLog :: (MonadIO m) => LoggingT m a -> m a
runLog action =
    runLoggingT action outputFor

main' :: App ()
main' = do
    logDebugN "Hello"
    liftIO $ print "Hello"

main :: IO ()
main = do
    let config = Config{}

    -- runReaderT (unApp main') config
    runLog $ runReaderT (unApp main') config

    return ()
