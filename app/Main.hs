module Main (main) where

import App
import Cli
import Control.Monad (forM_, when)
import Control.Monad.Logger.CallStack
import Control.Monad.Reader (ask)
import Danbooru (favPost)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import SauceNao (minimumSimilarity, querySauceNao)
import SauceNaoTypes
import System.Environment (getEnv)
import System.FilePath ((</>))
import UnliftIO
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory

main' :: App ()
main' = do
    config <- ask
    logInfo $ T.pack (show config)

    paths <- listDirectory config.inputFolder

    forM_ paths $ \p -> do
        path <- makeAbsolute (config.inputFolder </> p)
        logInfo $ "Tagging: " <> T.pack (show path)

        SauceNaoResponse{header, results} <- querySauceNao path

        let res = head results

        when (res.similarity >= minimumSimilarity) $ do
            logInfo $ "Faving: " <> T.pack (show res.id)
            favPost res.id

        logInfo $
            "Remaining short/long: "
                <> T.pack (show header.shortRemaining)
                <> "/"
                <> T.pack (show header.longRemaining)

        when (header.longRemaining == 0) $ do
            logError "longRemaining = 0"
            throwString "longRemaining = 0"

        when (header.shortRemaining == 0) $ do
            logWarn "Waiting for shortRemaining"
            threadDelay 35_000_000

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
