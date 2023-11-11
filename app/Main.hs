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
        P.zip inputFiles (inputFiles >-> mainSauce)
            >-> P.mapM_ (\x -> logInfo $ "main: " :# ["x" .= show x])
            >-> P.drain

    return ()
