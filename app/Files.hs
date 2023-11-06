{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Files where

import Blammo.Logging.Simple
import Cli
import Control.Monad.Reader
import Debug.Todo (todo)
import Main
import System.Directory (listDirectory)

inputFiles :: (MonadReader Env m, MonadIO m, MonadLogger m) => m [FilePath]
inputFiles = do
  env :: Env <- ask
  let inputFolder = env.cli.inputFolder
  res <- liftIO $ listDirectory inputFolder
  logInfo $ "inputFiles: " :# ["msg" .= res, "folder" .= inputFolder]
  return res
