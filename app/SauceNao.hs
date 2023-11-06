{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module SauceNao where

import Blammo.Logging.Simple
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson (FromJSON (parseJSON), Object, Options (fieldLabelModifier), decodeStrict, genericParseJSON)
import Data.Aeson.Types (defaultOptions)
import Data.ByteString
import Data.Text (Text)
import Debug.Todo (todo)
import GHC.Generics (Generic)
import Main (Env, sauceNaoApiKey)
import Network.HTTP.Client.MultipartFormData (partBS, partFile)
import Network.HTTP.Req

data SauceNaoResponse = SauceNaoResponse
  { header :: SauceNaoHeader
  , results :: [SauceNaoResult]
  }
  deriving (Show, Generic)

data SauceNaoHeader = SauceNaoHeader
  { account_type :: Text
  , long_remaining :: Integer
  , short_remaining :: Integer
  }
  deriving (Show, Generic)

data SauceNaoResult = SauceNaoResult
  { _header :: SauceNaoResultHeader
  , _data :: Object
  }
  deriving (Show, Generic)

data SauceNaoResultHeader = SauceNaoResultHeader
  { similarity :: Text
  , thumbnail :: Text
  }
  deriving (Show, Generic)

data SauceNaoResultData = SauceNaoResultData
  { danbooru_id :: Integer
  }
  deriving (Show, Generic)

instance FromJSON SauceNaoResponse

instance FromJSON SauceNaoHeader

instance FromJSON SauceNaoResult where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = Prelude.drop 1}

instance FromJSON SauceNaoResultHeader

testFile :: FilePath
testFile = "CAG_INPUT/test.jpg"

queryFile :: (MonadIO m, MonadLogger m, MonadReader Env m) => FilePath -> m SauceNaoResponse
queryFile file = do
  logInfo $ "Querying SauceNao" :# ["file" .= file]
  env <- ask
  let apikey :: Text = env.sauceNaoApiKey

  fileContents <- liftIO $ Data.ByteString.readFile file

  body :: ReqBodyMultipart <- reqBodyMultipart [partFile "file" file]

  let r :: Req BsResponse =
        req
          POST
          (https "saucenao.com" /: "search.php")
          body
          bsResponse
          $ ("output_type" =: ("2" :: Text))
            <> ("numres" =: ("1" :: Text))
            <> ("minsim" =: ("85" :: Text))
            <> ("db" =: ("9" :: Text))
            <> ("api_key" =: apikey)

  resp <- runReq defaultHttpConfig r
  logInfo $ "SauceNao response" :# ["response" .= show resp]

  let decoded :: Maybe SauceNaoResponse = decodeStrict $ responseBody resp
  logInfo $ "SauceNao decoded" :# ["decoded" .= show decoded]

  return undefined
