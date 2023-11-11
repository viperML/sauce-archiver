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
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Todo (todo)
import Network.HTTP.Client.MultipartFormData (partFile)
import Network.HTTP.Req
import Pipes
import qualified Pipes.Prelude as P
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
import UnliftIO (MonadUnliftIO, mapConcurrently)

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

fakeResult :: SauceResult
fakeResult = Right $ Sauce 1 1 1 1

batched :: (MonadUnliftIO m, MonadLogger m) => Int -> Int -> Pipe a a m ()
batched batchSize delay = do
    yield =<< await

    forever $ do
        mapM_ yield =<< replicateM (batchSize - 1) await

        n <- await
        lift $ logInfo $ "sleeping" :# ["ms" .= show delay]
        liftIO $ threadDelay delay
        yield n

mainSauce :: (MonadUnliftIO m, MonadLogger m, MonadReader Env m) => Pipe FilePath SauceResult m ()
mainSauce = do P.mapM queryFile >-> batched 3 30_500_000
