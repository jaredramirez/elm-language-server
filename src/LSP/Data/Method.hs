{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Method
  ( Method(..)
  , decode
  ) where

import           Data.Aeson           (FromJSON, Value, (.:))
import qualified Data.Aeson           as A
import           Data.Aeson.Types     (Parser)
import qualified Data.ByteString.Lazy as BS
import           Data.Text            (Text)

cancelRequest :: Text
cancelRequest = "$/cancelRequest"

textDocumentDefinition :: Text
textDocumentDefinition = "textDocument/definition"

textDocumentWillSaveWaitUntil :: Text
textDocumentWillSaveWaitUntil = "textDocument/willSaveWaitUntil"

clientRegisterCapability :: Text
clientRegisterCapability = "client/registerCapability"

clientUnregisterCapability :: Text
clientUnregisterCapability = "client/unregisterCapability"

data Method
  = CancelRequest
  | TextDocumentDefinition
  | TextDocumentWillSaveUntilWait
  | ClientRegisterCapablity
  | ClientUnregisterCapablity

methodDecoder :: Text -> Parser Method
methodDecoder key
  | key == cancelRequest = return CancelRequest
  | key == textDocumentDefinition = return TextDocumentDefinition
  | key == textDocumentWillSaveWaitUntil = return TextDocumentWillSaveUntilWait
  | key == clientRegisterCapability = return ClientRegisterCapablity
  | key == clientUnregisterCapability = return ClientUnregisterCapablity
  | otherwise = fail "Unknown method"

instance FromJSON Method where
  parseJSON = A.withText "Method" methodDecoder

decode :: BS.ByteString -> Either String Method
decode = A.eitherDecode'
