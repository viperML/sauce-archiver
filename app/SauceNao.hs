{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SauceNao where

import Blammo.Logging.Simple
import Cli
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson.Decoding (decodeStrict)
import Data.Functor (($>), (<&>))
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Todo (todo)
import Network.HTTP.Client.MultipartFormData (partFile)
import Network.HTTP.Req
import SauceNaoTypes (
    Sauce (
        Sauce,
        danbooru_id,
        long_remaining,
        short_remaining,
        similarity
    ),
    SauceError (Decode),
    SauceNaoHeader (long_remaining, short_remaining),
    SauceNaoResponse (..),
    SauceNaoResult (_data, _header),
    SauceNaoResultData (danbooru_id),
    SauceNaoResultHeader (similarity),
    SauceResult,
 )
import qualified System.Directory as System
import System.FilePath ((</>))
import UnliftIO (MonadUnliftIO, mapConcurrently, mapConcurrently_, replicateConcurrently_)
import Pipes (Producer)
import Pipes

testFile :: FilePath
testFile = "CAG_INPUT/test.jpg"

queryFile :: (MonadIO m, MonadLogger m, MonadReader Env m) => FilePath -> m SauceResult
queryFile file = do
    let ctx = ["file" .= file]
    logInfo $ "querying saucenao" :# ctx

    env <- ask
    let apikey = env.sauceNaoApiKey

    body :: ReqBodyMultipart <- reqBodyMultipart [partFile "file" file]

    let r :: Req BsResponse =
            req
                POST
                (https "saucenao.com" /: "search.php")
                body
                bsResponse
                $ ("output_type" =: ("2" :: Text))
                    <> ("numres" =: ("1" :: Text))
                    <> ("minsim" =: ("85" :: Text))
                    <> ("db" =: ("9" :: Text))
                    <> ("api_key" =: apikey)

    resp <- runReq defaultHttpConfig r
    logInfo $ "sauceNao response" :# ctx <> ["response" .= show resp]

    let decoded :: Maybe SauceNaoResponse = decodeStrict $ responseBody resp
    logInfo $ "sauceNao decoded" :# ctx <> ["decoded" .= decoded]

    case decoded of
        Just SauceNaoResponse{header = _header, results = [res]} ->
            return $
                Right $
                    Sauce
                        { danbooru_id = res._data.danbooru_id
                        , similarity = read (T.unpack res._header.similarity)
                        , short_remaining = _header.short_remaining
                        , long_remaining = _header.long_remaining
                        }
        _ -> do
            logError $ "saucenao fail to decode" :# ctx
            return $ Left Decode

queryFiles :: (MonadUnliftIO m, MonadLogger m, MonadReader Env m) => [FilePath] -> m [SauceResult]
queryFiles files =
    case splitAt 4 files of
        ([], _) -> return []
        (batch, []) -> do
            batchResults <- mapConcurrently queryFile batch
            mapM_ (\r -> logInfo $ "result" :# ["r" .= show r]) batchResults
            return batchResults
        (batch, rest) -> do
            batchResults <- queryFiles batch
            logWarn "waiting 31s for saucenao cooldown"

            replicateM_ 3 $ do
                liftIO $ threadDelay 10_000_000
                logWarn "waiting..."
            liftIO $ threadDelay 1_000_000

            restResults <- queryFiles rest

            return $ batchResults <> restResults

mainSauce :: (MonadUnliftIO m, MonadLogger m, MonadReader Env m) => m [(FilePath, SauceResult)]
mainSauce = do
    env <- ask
    let dir = env.cli.inputFolder
    _inputFiles <- liftIO $ System.listDirectory env.cli.inputFolder
    let inputFiles :: [FilePath] = _inputFiles <&> (dir </>)
    logInfo $ "reading input" :# ["inputFiles" .= inputFiles]

    zip inputFiles <$> queryFiles inputFiles


fakeResult :: SauceResult
fakeResult = Right $ Sauce 1 1 1 1

fakeQuery :: (MonadUnliftIO m, MonadLogger m) => Producer SauceResult m ()
fakeQuery = do
    replicateM_ 10 $ do
        yield fakeResult
        lift $ logInfo "fake result"

    lift $ logInfo "finish"

mainFake :: (MonadUnliftIO m, MonadLogger m) => m ()
mainFake = do
    logInfo "Begin pipe"

    runEffect $ fakeQuery >-> do
            n <- await
            lift $ logInfo $ "next" :# ["next" .= show n]

    logInfo "End pipe"