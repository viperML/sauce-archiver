{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main (main) where

import Blammo.Logging.Simple
import Data.Aeson
import Data.Text
import GHC.Generics
import Network.HTTP.Req

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

reqForPost :: (MonadHttp m, MonadLogger m) => Integer -> m BsResponse
reqForPost number =
  req
    GET
    (https "safebooru.donmai.us" /: "posts" /: pack (show number) <> ".json")
    NoReqBody
    bsResponse
    (header "User-Agent" "github/viperML")

decodeResponse :: BsResponse -> Either String Resp'
decodeResponse response = fixResp <$> eitherDecodeStrict (responseBody response)

runner :: LoggingT IO a -> IO a
runner = runSimpleLoggingT

main :: IO ()
main = runner $ do
  logInfo "Hello, world!"
  let r = reqForPost 6786211
  response <- runReq defaultHttpConfig r
  let resp = decodeResponse response
  logInfo $ "Response: " :# ["resp" .= resp]
  return ()
