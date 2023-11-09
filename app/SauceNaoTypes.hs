{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module SauceNaoTypes where

import Data.Aeson (Object)
import Data.Aeson.TH
import Data.Text (Text)

data SauceNaoResultHeader = SauceNaoResultHeader
    {similarity :: Text, thumbnail :: Text}
    deriving (Show)

deriveJSON defaultOptions ''SauceNaoResultHeader

data SauceNaoResultData = SauceNaoResultData
    {danbooru_id :: Integer}
    deriving (Show)

deriveJSON defaultOptions ''SauceNaoResultData

data SauceNaoResult = SauceNaoResult
    { _header :: SauceNaoResultHeader
    , _data :: SauceNaoResultData
    }
    deriving (Show)

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''SauceNaoResult

data SauceNaoHeader = SauceNaoHeader
    { account_type :: Text
    , long_remaining :: Integer
    , short_remaining :: Integer
    }
    deriving (Show)

deriveJSON defaultOptions ''SauceNaoHeader

data SauceNaoResponse = SauceNaoResponse
    { header :: SauceNaoHeader
    , results :: [SauceNaoResult]
    }
    deriving (Show)

deriveJSON defaultOptions ''SauceNaoResponse

data SauceError
    = NoResults
    | ShortTimeout
    | LongTimeout
    | Decode
    deriving (Show)

data Sauce = Sauce
    { similarity :: Float
    , danbooru_id :: Integer
    , long_remaining :: Integer
    , short_remaining :: Integer
    }
    deriving (Show)

type SauceResult = Either SauceError Sauce
