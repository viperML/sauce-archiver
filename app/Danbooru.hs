module Danbooru where

import App (App, Config (danbooru_apikey, danbooru_username))
import Control.Monad.Reader
import Network.HTTP.Req
import Control.Monad.Logger (logInfoN)
import Data.Text.Encoding (decodeUtf8)

favPost :: Integer -> App ()
favPost postId = do
    config <- ask

    let r =
            req
                POST
                (https "danbooru.donmai.us" /: "favorites.json")
                NoReqBody
                bsResponse
                $ ("post_id" =: show postId)
                    <> ("login" =: danbooru_username config)
                    <> ("api_key" =: danbooru_apikey config)
                    <> header "User-Agent" "curl/8.9.1"
                    <> header "Content-Type" "application/json"

    response <- runReq defaultHttpConfig r
    let body = responseBody response
    let t = decodeUtf8 body
    logInfoN t

    return ()
