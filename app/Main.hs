module Main (main) where

import App
import Cli
import Control.Monad (forM_)
import Control.Monad.Logger.CallStack
import Control.Monad.Reader (ask)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Environment (getEnv)
import UnliftIO
import UnliftIO.Directory

main' :: App ()
main' = do
    config <- ask
    logInfo $ T.pack (show config)

    files <- listDirectory config.inputFolder
    forM_ files $ \f -> do
        logInfo $ "Tagging: " <> T.pack (show f)

main :: IO ()
main = do
    cli <- getCliOptions

    config <-
        Config
            <$> getEnv "DANBOORU_USERNAME"
            <*> getEnv "DANBOORU_APIKEY"
            <*> getEnv "SAUCENAO_APIKEY"
            <*> return (fromMaybe "CAG_INPUT" cli.inputFolder)
            <*> return (fromMaybe "CAG_SAUCE" cli.sauceFolder)
            <*> return (fromMaybe "CAG_NOSAUCE" cli.noSauceFolder)

    runApp config main'
