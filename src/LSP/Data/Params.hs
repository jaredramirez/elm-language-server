{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Params
  ( Params(..)
  ) where

import           Control.Applicative             ((<|>))
import           Data.Aeson                      (FromJSON, Value, (.:))
import qualified Data.Aeson                      as A
import qualified Data.ByteString.Lazy            as BS
import           Data.Text                       (Text)
import           LSP.Data.Position               (Position)
import           LSP.Data.Registration           (Registration)
import           LSP.Data.TextDocumentIdentifier (TextDocumentIdentifier)
import           LSP.Data.Unregistration         (Unregistration)
import           Misc                            ((<|))

-- TextDocumentPositionParams --
newtype TextDocumentPositionParams =
  TextDocumentPositionParams (TextDocumentIdentifier, Position)

instance FromJSON TextDocumentPositionParams where
  parseJSON =
    A.withObject "TextDocumentPositionParams" <| \v ->
      curry TextDocumentPositionParams <$> v .: "textDocument" <*>
      v .: "position"

-- RegistrationParams --
newtype RegistrationParams =
  RegistrationParams [Registration]

instance FromJSON RegistrationParams where
  parseJSON =
    A.withObject "Registration" $ \v ->
      RegistrationParams <$> v .: "registrations"

-- UnregistrationParams --
newtype UnregistrationParams =
  UnregistrationParams [Unregistration]

instance FromJSON UnregistrationParams where
  parseJSON =
    A.withObject "UnregistrationParams" $ \v ->
      UnregistrationParams <$> v .: "unregisterations"

-- Params --
data Params
  = TextDocumentPosition TextDocumentPositionParams
  | Registration RegistrationParams
  | Unregistration UnregistrationParams

instance FromJSON Params where
  parseJSON v =
    (TextDocumentPosition <$> A.parseJSON v) <|>
    (Registration <$> A.parseJSON v) <|>
    (Unregistration <$> A.parseJSON v)
