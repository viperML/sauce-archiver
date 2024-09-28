module Main (main) where

import App
import System.Environment (getEnv)
import SauceNao
import UnliftIO
import Danbooru (favPost)

app :: App ()
app = do
    post <- liftIO $ read <$> getEnv "POST"
    favPost post

    return ()

main :: IO ()
main = do
    config <-
        Config <$> getEnv "DANBOORU_USERNAME" <*> getEnv "DANBOORU_APIKEY" <*> getEnv "SAUCENAO_APIKEY"

    runApp config app

    return ()
