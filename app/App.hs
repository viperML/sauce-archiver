{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module App where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, logDebugN, logInfoN)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Log (runLog)
import UnliftIO (MonadUnliftIO)

data Config = Config
    { danbooru_username :: String
    , danbooru_apikey :: String
    , saucenao_apikey :: String
    }
    deriving (Show)

newtype App a = App
    { unApp :: ReaderT Config (LoggingT IO) a
    }
    deriving newtype
        (Functor, Applicative, Monad, MonadLogger, MonadReader Config, MonadIO, MonadUnliftIO)


runApp :: Config -> App a -> IO a
runApp config x = runLog $ runReaderT (unApp x) config
