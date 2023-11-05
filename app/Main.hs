{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main (main) where

import Blammo.Logging.Simple
import Data.Aeson
import Data.Text
import Debug.Todo (todo)
import GHC.Generics
import Network.HTTP.Req
import Data.Text.Encoding (decodeUtf8)
import Control.Monad.IO.Class (MonadIO)

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

runner :: LoggingT IO a -> IO a
runner = runSimpleLoggingT

main :: IO ()
main = runner $ do
  logInfo "Hello, world!"
  r <- reqForPost' 6786211
  return ()
