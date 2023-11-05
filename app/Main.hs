{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Blammo.Logging.Simple
import Cli (CliArgs, readCli)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Aeson
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Debug.Todo (todo)
import GHC.Generics
import Network.HTTP.Req
import Prelude hiding (id)

data Resp = Resp
  { id :: Integer,
    tag_string_general :: Text,
    file_url :: Text
  }
  deriving (Show, Generic)

data Resp' = Resp'
  { id :: Integer,
    tag_string_general :: [Text],
    file_url :: Text
  }
  deriving (Show, Generic)

instance FromJSON Resp

instance ToJSON Resp'

fixResp :: Resp -> Resp'
fixResp Resp {id, tag_string_general, file_url} = Resp' {id, tag_string_general = splitOn " " tag_string_general, file_url}

reqForPost :: Integer -> Req BsResponse
reqForPost number =
  req
    GET
    (https "safebooru.donmai.us" /: "posts" /: pack (show number) <> ".json")
    NoReqBody
    bsResponse
    (header "User-Agent" "github/viperML")

reqForPost' :: (MonadIO m, MonadLogger m) => Integer -> m BsResponse
reqForPost' number = do
  resp <- runReq defaultHttpConfig $ reqForPost number
  logInfo $ decodeUtf8 (responseBody resp) :# []
  return resp

decodeResponse :: BsResponse -> Either String Resp'
decodeResponse response = fixResp <$> eitherDecodeStrict (responseBody response)

logRunner :: LoggingT IO a -> IO a
logRunner = runSimpleLoggingT

data Env = Env
  { cli :: CliArgs
  }
  deriving (Show, Generic)

type App = LoggingT (ReaderT Env IO)

runApp :: App a -> IO a
runApp app = do
  cli <- readCli
  let env = Env {cli}

  runReaderT
    (runSimpleLoggingT app)
    env

main :: IO ()
main = runApp main'

main' :: App ()
main' = do
  env <- lift ask
  logInfo $ "reading env" :# ["env" .= show env]
  logInfo "Hello world!"
