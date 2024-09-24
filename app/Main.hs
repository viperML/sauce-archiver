module Main (main) where

import App
import System.Environment (getEnv)
import SauceNao
import UnliftIO

app :: App ()
app = do
    res <- query _pic
    liftIO $ print $ show res

    return ()

main :: IO ()
main = do
    config <-
        Config <$> getEnv "DANBOORU_USERNAME" <*> getEnv "DANBOORU_APIKEY" <*> getEnv "SAUCENAO_APIKEY"

    runApp config app

    return ()
