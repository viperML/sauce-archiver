{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader
import Log (runLog)
import Control.Monad.Logger (LoggingT, MonadLogger, logDebugN, logInfoN)

data Config = Config
    {
    }
    deriving (Show)

newtype App a = App
    { unApp :: ReaderT Config (LoggingT IO) a
    }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader Config)


main' :: App ()
main' = do
    logDebugN "Hello"
    logInfoN "Goodbye"
    liftIO $ print "Hello"

main :: IO ()
main = do
    let config = Config{}

    -- runReaderT (unApp main') config
    runLog $ runReaderT (unApp main') config

    return ()
