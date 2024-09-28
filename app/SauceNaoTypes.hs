{-# LANGUAGE DeriveGeneric #-}

module SauceNaoTypes where

import Control.Monad (MonadPlus (mzero))
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.Types as JSON
import qualified Data.Text as T
import GHC.Generics
import Text.Read (readMaybe)

data SauceNaoResponse = SauceNaoResponse
    { header :: SauceNaoHeader
    , results :: [SauceNaoResult]
    }
    deriving (Generic, Show)

data SauceNaoHeader = SauceNaoHeader
    { shortRemaining :: Integer
    , longRemaining :: Integer
    }
    deriving (Generic, Show)

instance FromJSON SauceNaoHeader where
    parseJSON (Object o) =
        SauceNaoHeader
            <$> (o .: "short_remaining")
            <*> (o .: "long_remaining")
    parseJSON _ = mzero

data SauceNaoResult = SauceNaoResult
    { similarity :: Double
    , id :: Integer
    }
    deriving (Show)

instance FromJSON SauceNaoResponse

instance FromJSON SauceNaoResult where
    parseJSON (Object o) =
        let
            parseSim :: Parser String = ((o .: "header") >>= (.: "similarity"))
            parseSim' :: Parser Double = do
                s <- parseSim
                case readMaybe s of
                    Nothing -> typeMismatch "String" (JSON.String (T.pack s))
                    Just d -> return d
         in
            SauceNaoResult
                <$> parseSim'
                <*> ((o .: "data") >>= (.: "danbooru_id"))
    parseJSON _ = mzero
