{-# LANGUAGE OverloadedStrings #-}

module Files where
import Control.Monad.IO.Class (MonadIO)
import Debug.Todo (todo)
import System.Directory (listDirectory)

inputFolder :: FilePath
inputFolder = "CAG_INPUT"

inputFiles :: IO [FilePath]
inputFiles = listDirectory inputFolder