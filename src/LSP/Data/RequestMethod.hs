{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.RequestMethod
  ( InitializeParams(..)
  , RequestMethod(..)
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

-- SHUTDOWN --
shutdown :: Text
shutdown = "shutdown"

-- METHODS --
data RequestMethod
  = Initialize Value
  | Shutdown
  deriving (Show)

decoder :: HM.HashMap Text Value -> Text -> Parser RequestMethod
decoder v key
  | key == initialize = Initialize <$> v .: "params"
  | key == shutdown = return Shutdown
  | otherwise = fail "Unknown request method"

instance FromJSON RequestMethod where
  parseJSON = A.withObject "RequestMethod" <| \v -> v .: "method" >>= decoder v
