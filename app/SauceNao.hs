{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module SauceNao where

import Blammo.Logging.Simple
import Cli
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson.Decoding (decodeStrict)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Todo (todo)
import Network.HTTP.Client.MultipartFormData (partFile)
import Network.HTTP.Req
import SauceNaoTypes
    ( SauceNaoResultData(danbooru_id),
      SauceNaoResultHeader(similarity),
      SauceNaoResult(_header, _data),
      SauceNaoHeader(long_remaining, short_remaining),
      SauceNaoResponse(..),
      SauceResult,
      Sauce(Sauce, danbooru_id, similarity, short_remaining,
            long_remaining),
      SauceError(Decode) )
import qualified System.Directory as System
import UnliftIO (MonadUnliftIO, mapConcurrently, mapConcurrently_, replicateConcurrently_)

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

fakeQueryFile :: (MonadUnliftIO m, MonadLogger m, MonadReader Env m) => FilePath -> m SauceResult
fakeQueryFile file = do
    logInfo "Querying SauceNao"

    let newFileName = file <> ".test"

    liftIO $ writeFile newFileName "fake query start"

    return $ Right $ Sauce 0 0 0 0

mainSauce :: (MonadUnliftIO m, MonadLogger m, MonadReader Env m) => m [(FilePath, SauceResult)]
mainSauce = do
    env <- ask
    inputFiles <- liftIO $ System.listDirectory env.cli.inputFolder
    logInfo $ "reading input" :# ["inputFiles" .= inputFiles]

    -- let x = files.inputFolder

    return $! todo "FIXME"

-- stuff :: (MonadUnliftIO m, MonadLogger m, MonadReader Env m) => Int -> m [Bool]
-- stuff n = do
--     let files :: [FilePath] = Prelude.map (\i -> "test-" <> show i) [1 .. n]

--     res <- fakeQueryConcurrent files
--     logInfo $ "Result" :# ["res" .= show res]

--     return []

-- fakeQueryConcurrent :: (MonadUnliftIO m, MonadLogger m) => [FilePath] -> m [Bool]
-- fakeQueryConcurrent files = do
--     case files of
--         (f1 : f2 : rest) -> do
--             x <- mapConcurrently fakeQuery [f1, f2]
--             let y = x $> True
--             liftIO $ threadDelay 3_000_000
--             restRes <- case rest of
--                 [] -> return []
--                 _ -> fakeQueryConcurrent rest
--             return $ y <> restRes
--         f -> do
--             res <- mapConcurrently fakeQuery f
--             return $ res $> True
