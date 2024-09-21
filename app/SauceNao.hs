{-# LANGUAGE ScopedTypeVariables #-}

module SauceNao where

import Network.HTTP.Req (
    BsResponse,
    MonadHttp,
    NoReqBody (NoReqBody),
    POST (POST),
    QueryParam (queryParam),
    Scheme,
    Url,
    bsResponse,
    header,
    https,
    req,
    (/:),
    (=:), reqBodyMultipart, ReqBodyMultipart, runReq, defaultHttpConfig,
 )

import Data.ByteString as BS
import Data.Text (Text)
import Network.HTTP.Client.MultipartFormData (partFile)
import UnliftIO
import System.Environment (getEnv)

b :: Text
b = "x"


pic :: FilePath
pic = "/var/home/ayats/Pictures/pic.jpg"

multipart :: MonadIO m => m ReqBodyMultipart
multipart = reqBodyMultipart [(partFile "file" pic)]

_apikey :: IO String
_apikey = getEnv "SAUCENAO_APIKEY"

reqFor :: (MonadHttp m, MonadIO i) => String -> i (m BsResponse)
reqFor apikey = do
    mp <- multipart
    return $ req
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

check :: IO BsResponse
check = do
    apikey <- _apikey
    r <- reqFor apikey
    runReq defaultHttpConfig r
