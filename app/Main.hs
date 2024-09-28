{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import App
import Cli
import Danbooru (favPost)
import qualified Data.Text as T
import SauceNao
import SauceNaoTypes
import System.Environment (getEnv)
import UnliftIO
import Control.Monad.Logger

app :: App ()
app = do
    o <- liftIO options
    let f = file o
    $(logInfo) "Starting"
    logInfoN "Other"
    _ <- throwString "FIXME"

    sauceResult <- querySauceNao f

    let first = head sauceResult.results

    logInfoN (T.pack $ show first)

    if first.similarity >= minimumSimilarity
        then logInfoN "Faving" >> favPost first.id
        else throwString "Similarty wasn't greater than min"

    return ()

main :: IO ()
main = do
    config <-
        Config <$> getEnv "DANBOORU_USERNAME" <*> getEnv "DANBOORU_APIKEY" <*> getEnv "SAUCENAO_APIKEY"

    runApp config app

    return ()
