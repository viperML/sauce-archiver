{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant return" #-}

module Main where

import Blammo.Logging.Simple
import Cli

import Control.Monad.Reader
import Danbooru (favPost, sauceNaoAdapter)
import Files (inputFiles)
import Pipes
import qualified Pipes.Prelude as P
import SauceNao (mainSauce)
import UnliftIO (MonadUnliftIO)
import Prelude hiding (id)

main :: IO ()
main = runApp main'

main' :: (MonadUnliftIO m, MonadLogger m, MonadReader Env m) => m ()
main' = do
    env <- ask
    logInfo $ "reading env" :# ["env" .= show env]

    runEffect $
        inputFiles
            >-> mainSauce
            >-> pipeLog
            >-> P.map sauceNaoAdapter
            >-> P.mapM_ favPost
            >-> P.drain

    return ()

pipeLog :: (MonadLogger m, Show a) => Pipe a a m ()
pipeLog = forever $ do
    x <- await
    lift $ logInfo $ "pipeLog: " :# ["x" .= show x]
    yield x
