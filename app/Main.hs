{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Cli
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently))
import Control.Exception (throwIO)
import Control.Monad (forever)
import Control.Monad.Catch (MonadCatch, MonadThrow (throwM), catch)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, logDebugN, logInfoN)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson (FromJSON, Value (String))
import Data.Aeson.Types (Value)
import qualified Data.Text as T
import GHC.IO.Exception (IOException (IOError))
import GHC.Stack (HasCallStack)
import Log (runLog)
import Network.HTTP.Client
import Network.HTTP.Req
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)

data Config = Config
    { username :: String
    , apikey :: String
    }
    deriving (Show)

newtype App a = App
    { unApp :: ReaderT Config (LoggingT IO) a
    }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader Config, MonadThrow, MonadCatch)

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

    return $ https "testbooru.donmai.us" /: "posts" /: "6.json"

doReq :: (MonadHttp m, FromJSON a) => Url 'Https -> m (JsonResponse a)
doReq url =
    req
        GET
        url
        NoReqBody
        jsonResponse
        $ header "User-Agent" "viperML"

doReq2 :: (HasCallStack) => App ()
doReq2 = do
    logInfoN "Hello"

    x <-
        ( req
                GET
                (https "testbooru.donmai.us" /: "wtf")
                NoReqBody
                ignoreResponse
                $ header "User-Agent" "viperML"
            )
            `catch` ( \case
                        VanillaHttpException (HttpExceptionRequest r (StatusCodeException a b)) -> undefined
                        other -> throwM other
                    )

    return ()

main :: IO ()
main = do
    -- config <- Config <$> getEnv "DANBOORU_USERNAME" <*> getEnv "DANBOORU_APIKEY"
    -- print config
    --
    res <-
        (Right <$> getEnv "never")
            `catch` ( \(e :: IOError) ->
                        if isDoesNotExistError e
                            then return $ Left ""
                            else throwM e
                    )

    print res

    return ()
