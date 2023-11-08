{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Scratch where

import Control.Monad.IO.Class
import Control.Monad.State

data Bucket = Bucket
  { remaining :: Int
  }
  deriving (Show)

queryBucket :: (MonadIO m, MonadState Bucket m) => m ()
queryBucket = do
  replicateM_ 5 $ do
    s <- get
    put $ s {remaining = remaining s - 1}

  s <- get
  liftIO $ putStrLn $ "Remaining: " <> show s.remaining

  return ()

runBucket :: IO ()
runBucket = do
  let bucket = Bucket{remaining = 5}

  x <- execStateT queryBucket bucket

  putStrLn $ "Remaining: " <> show x

