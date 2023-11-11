{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Danbooru where

import Blammo.Logging (logDebug, logInfo)
import Blammo.Logging.Simple
import Cli
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Todo (todo)
import Network.HTTP.Req
import SauceNaoTypes (Sauce (..))
import UnliftIO (MonadUnliftIO, liftIO)

sauceNaoAdapter :: Sauce -> Int
sauceNaoAdapter s = fromIntegral s.danbooru_id

favPost :: (MonadUnliftIO m, MonadReader Env m, MonadLogger m) => Int -> m ()
favPost number = do
    ctx <- ask

    let r :: Req BsResponse =
            req
                GET
                (https "danbooru.donmai.us" /: "favorites.json")
                NoReqBody
                bsResponse
                $ ("post_id" =: number)
                    <> ("login" =: ctx.danbooruUsername)
                    <> ("api_key" =: ctx.danbooruApiKey)
                    <> header "User-Agent" "github-viperML-sauce-archiver"

    logInfo $ "adding favorite" :# ["post" .= number]
    resp <- liftIO $ runReq defaultHttpConfig r
    logInfo $ "favPost response: " :# ["r" .= show resp]

    return ()
