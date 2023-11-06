{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-cse #-}

module Cli (readCli, CliArgs (..), cliArgs) where

import System.Console.CmdArgs

data CliArgs = CliArgs
  { inputFolder :: FilePath
  , sauceFolder :: FilePath
  , nosauceFolder :: FilePath
  }
  deriving (Show, Data, Typeable)

cliArgs :: CliArgs
cliArgs =
  CliArgs
    { inputFolder = "CAG_INPUT"
    , sauceFolder = "CAG_SAUCE"
    , nosauceFolder = "CAG_NOSAUCE"
    }

readCli :: IO CliArgs
readCli = cmdArgs cliArgs
