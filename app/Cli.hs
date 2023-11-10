{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-cse #-}

module Cli where

import Blammo.Logging.Simple
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import System.Console.CmdArgs
import System.Environment (getEnv)

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

runApp :: LoggingT (ReaderT Env IO) a -> IO a
runApp app = do
    cli <- readCli
    sauceNaoApiKey <- T.pack <$> getEnv "SAUCENAO_APIKEY"
    let env = Env{cli, sauceNaoApiKey}

    runReaderT
        (runSimpleLoggingT app)
        env
