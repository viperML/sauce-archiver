{-# LANGUAGE ScopedTypeVariables #-}

module SauceNao where

import Network.HTTP.Req (
    BsResponse,
    MonadHttp,
    NoReqBody (NoReqBody),
    POST (POST),
    QueryParam (queryParam),
    ReqBodyMultipart,
    Scheme,
    Url,
    bsResponse,
    defaultHttpConfig,
    header,
    https,
    req,
    reqBodyMultipart,
    responseBody,
    runReq,
    (/:),
    (=:),
 )

import App (App, Config (saucenao_apikey))
import Control.Monad.Logger (logDebugN, logInfo)
import Control.Monad.Reader (ask)
import Data.Aeson
import Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client.MultipartFormData (partFile)
import SauceNaoTypes (SauceNaoResponse (SauceNaoResponse))
import System.Environment (getEnv)
import UnliftIO

_pic :: FilePath
_pic = "/var/home/ayats/Pictures/pic.jpg"

_apikey :: IO String
_apikey = getEnv "SAUCENAO_APIKEY"

minimumSimilarity :: Double
minimumSimilarity = 85.0

querySauceNao :: FilePath -> App SauceNaoResponse
querySauceNao path = do
    config <- ask
    let apikey = config.saucenao_apikey

    body <- reqBodyMultipart [partFile "file" path]

    logDebugN "querying saucenao"

    r <-
        runReq defaultHttpConfig
            $ req
                POST
                (https "saucenao.com" /: "search.php")
                body
                bsResponse
            $ ("output_type" =: ("2" :: String))
                <> ("numres" =: ("1" :: String))
                <> ("minsim" =: minimumSimilarity)
                <> ("db" =: ("9" :: String))
                <> ("api_key" =: apikey)
                <> header "User-Agent" "curl/8.9.1"

    let body = responseBody r
    let parsed :: Maybe SauceNaoResponse = decodeStrict body
    logDebugN $ T.pack $ show parsed

    case parsed of
        Just res -> return res
        Nothing -> logDebugN (decodeUtf8 body) >> throwString "Failed to parse"
