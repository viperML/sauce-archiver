{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Files where

import Blammo.Logging.Simple
import Cli (CliArgs, inputFolder)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ask)
import Debug.Todo (todo)
import Main (App, Env (Env), cli)
import System.Directory (listDirectory)

inputFiles :: App [FilePath]
inputFiles = do
  env <- lift ask
  let inputFolder = env.cli.inputFolder
  res <- liftIO $ listDirectory inputFolder
  logInfo $ "inputFiles: " :# ["msg" .= res, "folder" .= inputFolder]
  return res
