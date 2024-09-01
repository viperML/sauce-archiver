{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main (main) where

import Cli
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (forever)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, logDebugN, logInfoN)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson (FromJSON, Value (String))
import Data.Aeson.Types (Value)
import Data.Functor
import qualified Data.Text as T
import GHC.IO.Exception (IOException (IOError))
import GHC.Stack (HasCallStack)
import Log (runLog)
import Network.HTTP.Client
import Network.HTTP.Req
import Options.Applicative (helpLongEquals)
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async

data Config = Config
    { username :: String
    , apikey :: String
    }
    deriving (Show)

newtype App a = App
    { unApp :: ReaderT Config (LoggingT IO) a
    }
    deriving newtype
        (Functor, Applicative, Monad, MonadLogger, MonadReader Config, MonadIO, MonadUnliftIO)

-- instance MonadHttp App where
--     handleHttpException = liftIO . throwIO

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

    return $ https "testbooru.donmai.us" /: "posts" /: "6.json"

doReq :: (MonadHttp m, FromJSON a) => Url 'Https -> m (JsonResponse a)
doReq url =
    req
        GET
        url
        NoReqBody
        jsonResponse
        $ header "User-Agent" "viperML"

doReq3 :: App ()
doReq3 = do
    liftIO $ print "start"
    liftIO $ threadDelay (1500 * 1000)
    liftIO $ print "finish"

x :: App ()
x = void (runConcurrently ((,) <$> Concurrently doReq3 <*> Concurrently doReq3))

main :: IO ()
main = do
    config <- Config <$> getEnv "DANBOORU_USERNAME" <*> getEnv "DANBOORU_APIKEY"

    runApp config x

    return ()
