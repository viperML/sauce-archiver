{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Cli
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently))
import Control.Exception (throwIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, logDebugN, logInfoN)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson (FromJSON)
import Data.Aeson.Types (Value)
import qualified Data.Text as T
import Log (runLog)
import Network.HTTP.Req
import System.Environment (getEnv)

data Config = Config
    { username :: String
    , apikey :: String
    }
    deriving (Show)

newtype App a = App
    { unApp :: ReaderT Config (LoggingT IO) a
    }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader Config)
instance MonadHttp App where
    handleHttpException = liftIO . throwIO

runApp :: Config -> App a -> IO a
runApp config x = runLog $ runReaderT (unApp x) config

doStuff :: Int -> IO ()
doStuff x = forever $ do
    print $ "doing " <> show x
    threadDelay $ x * 1000

action :: IO ()
action =
    runConcurrently $
        Concurrently (doStuff 250)
            *> Concurrently (doStuff 1000)

myUrl :: IO (Url 'Https)
myUrl = do
    username <- getEnv "DANBOORU_USERNAME"
    apikey <- getEnv "DANBOORU_APIKEY"

    let x = https "testbooru.donmai.us" /: "posts" /: "6.json"

    return x

doReq :: (MonadHttp m, FromJSON a) => Url 'Https -> m (JsonResponse a)
doReq url =
    req
        GET
        url
        NoReqBody
        jsonResponse
        $ header "User-Agent" "viperML"

doReq2 :: App ()
doReq2 = do
    logInfoN "Hello"

    x :: JsonResponse Value <-
        req
            GET
            (https "testbooru.donmai.us" /: "posts" /: "6.json")
            NoReqBody
            jsonResponse
            $ header "User-Agent" "viperML"

    logInfoN $ T.pack $ show x

    return ()

main :: IO ()
main = do
    config <- Config <$> getEnv "DANBOORU_USERNAME" <*> getEnv "DANBOORU_APIKEY"
    print config

    runApp config doReq2

    return ()
