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
import Data.Aeson
import Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client.MultipartFormData (partFile)
import SauceNaoTypes (SauceNaoResponse (SauceNaoResponse))
import System.Environment (getEnv)
import UnliftIO
import Control.Monad.Reader (ask)

b :: Text
b = "x"

_pic :: FilePath
_pic = "/var/home/ayats/Pictures/pic.jpg"

_apikey :: IO String
_apikey = getEnv "SAUCENAO_APIKEY"

reqFor :: (MonadHttp m) => String -> ReqBodyMultipart -> m BsResponse
reqFor apikey mp =
    req
        POST
        (https "saucenao.com" /: "search.php")
        mp
        bsResponse
        $ ("output_type" =: ("2" :: String))
            <> ("numres" =: ("1" :: String))
            <> ("minsim" =: ("85" :: String))
            <> ("db" =: ("9" :: String))
            <> ("api_key" =: apikey)
            <> (header "User-Agent" "curl/8.9.1")

query :: FilePath -> App SauceNaoResponse
query pic = do
    config <- ask
    let apikey = saucenao_apikey config

    mp <- reqBodyMultipart [partFile "file" pic]

    logDebugN "querying saucenao"
    r <- runReq defaultHttpConfig $ reqFor apikey mp

    let body = responseBody r
    let parsed :: Maybe SauceNaoResponse = decodeStrict body
    logDebugN $ T.pack $ show parsed

    case parsed of
        Just res -> return res
        Nothing -> throwString "Failed to parse"
