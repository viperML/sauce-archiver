{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Cli where

import System.Console.CmdArgs
import GHC.Generics
import Data.Text (Text)
import Control.Monad.Reader (ReaderT)
import Blammo.Logging.Simple
import System.Environment (getEnv)
import qualified Data.Text as T
import Control.Monad.Trans.Reader (runReaderT)

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

logRunner :: LoggingT IO a -> IO a
logRunner = runSimpleLoggingT

data Env = Env
  { cli :: CliArgs
  , sauceNaoApiKey :: Text
  }
  deriving (Show, Generic)

type App = LoggingT (ReaderT Env IO)

runApp :: App a -> IO a
runApp app = do
  cli <- readCli
  sauceNaoApiKey <- T.pack <$> getEnv "SAUCENAO_APIKEY"
  let env = Env{cli, sauceNaoApiKey}

  runReaderT
    (runSimpleLoggingT app)
    env

