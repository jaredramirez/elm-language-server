{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Registration
  ( Registration(..)
  ) where

import           Control.Applicative       ((<|>))
import           Data.Aeson                (FromJSON, ToJSON, Value, (.:),
                                            (.:?))
import qualified Data.Aeson                as A
import qualified Data.ByteString.Lazy      as BS
import           Data.Text                 (Text)
import           LSP.Data.DocumentSelector (DocumentSelector)
import           LSP.Data.Method           (Method)
import qualified Misc

-- REGISTATION OPTIONS --
newtype TextDocumentRegistrationOptions =
  TextDocumentRegistrationOptions (Maybe DocumentSelector)

instance FromJSON TextDocumentRegistrationOptions where
  parseJSON =
    A.withObject "TextDocumentRegistrationOptions" $ \v ->
      TextDocumentRegistrationOptions <$> v .:? "documentSelector"

-- OPTIONS --
data Options =
  TextDocumentRegistration TextDocumentRegistrationOptions

instance FromJSON Options where
  parseJSON v = TextDocumentRegistration <$> A.parseJSON v

-- REGISTATION --
newtype Registration =
  Registration (Text, Method, Maybe Options)

instance FromJSON Registration where
  parseJSON =
    A.withObject "Registration" $ \v ->
      Misc.curryTriple Registration <$> v .: "id" <*> v .: "method" <*>
      v .:? "options"

decode :: BS.ByteString -> Either String Registration
decode = A.eitherDecode'
