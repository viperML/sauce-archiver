module Danbooru where

import App
import Control.Monad.Logger.CallStack
import Control.Monad.Reader
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Req

favPost :: Integer -> App ()
favPost postId = do
    config <- ask

    response <-
        runReq defaultHttpConfig
            $ req
                POST
                (https "danbooru.donmai.us" /: "favorites.json")
                NoReqBody
                bsResponse
            $ ("post_id" =: show postId)
                <> ("login" =: danbooruUsername config)
                <> ("api_key" =: danbooruApikey config)
                <> header "User-Agent" "curl/8.9.1"
                <> header "Content-Type" "application/json"

    let resp = decodeUtf8 (responseBody response)
    logDebug $ "Danbooru response: " <> resp

    return ()
