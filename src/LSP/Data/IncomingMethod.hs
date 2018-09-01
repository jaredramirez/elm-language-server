{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.IncomingMethod
  ( IncomingMethod(..)
  , decode
  ) where

import           Data.Aeson           (FromJSON, Value, (.:))
import qualified Data.Aeson           as A
import           Data.Aeson.Types     (Parser)
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict  as HM
import           Data.Text            (Text)
import           Misc                 ((<|))

-- INITIALIZE --
initialize :: Text
initialize = "initialize"

newtype InitializeParams =
  InitializeParams Text
  deriving (Show)

instance FromJSON InitializeParams where
  parseJSON =
    A.withObject "InitializeParams" <| \v -> InitializeParams <$> v .: "rootUri"

-- METHODS --
data IncomingMethod =
  Initialize InitializeParams
  deriving (Show)

incomingMethodDecoder :: HM.HashMap Text Value -> Text -> Parser IncomingMethod
incomingMethodDecoder v key
  | key == initialize = Initialize <$> v .: "params"
  | otherwise = fail "Unknown method"

instance FromJSON IncomingMethod where
  parseJSON =
    A.withObject "IncomingMethod" <| \v ->
      v .: "method" >>= incomingMethodDecoder v

decode :: BS.ByteString -> Either String IncomingMethod
decode = A.eitherDecode'
