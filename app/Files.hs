{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Files where

import Blammo.Logging.Simple
import Cli
import Control.Monad.Reader
import Pipes
import qualified Pipes.Prelude as P
import System.Directory (listDirectory)
import System.FilePath
import UnliftIO (MonadUnliftIO)

inputFiles :: (MonadUnliftIO m, MonadReader Env m) => Producer FilePath m ()
inputFiles = do
    env <- ask

    files <- liftIO $ listDirectory env.cli.inputFolder
    let filesFixed = (env.cli.inputFolder </>) <$> files

    each filesFixed
