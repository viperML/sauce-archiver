{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Blammo.Logging.Simple
import Cli

import Control.Monad.Reader
import SauceNao (mainSauce)
import Prelude hiding (id)

main :: IO ()
main = runApp main'

main' :: App ()
main' = do
    env <- lift ask
    logInfo $ "reading env" :# ["env" .= show env]
    _ <- mainSauce
    return ()
