{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.NotificationMethod
  ( NotificationMethod(..)
  , TextDocumentDidOpenParams(..)
  , PublishDiagnosticsParams(..)
  , ContentChange(..)
  , TextDocumentDidChangeParams(..)
  , TextDocumentDidSaveParams(..)
  , RegisterCapabilityParams(..)
  , DidChangeWatchedFilesParams(..)
  , toPairs
  ) where

import           Data.Aeson            (ToJSON, FromJSON, Value, (.:), (.:?), (.=))
import qualified Data.Aeson            as A
import           Data.Aeson.Types      (Parser, Pair)
import qualified Data.ByteString.Lazy  as BS
import qualified Data.HashMap.Strict   as HM
import           Data.Text             (Text)
import           LSP.Data.FileEvent    (FileEvent)
import           LSP.Data.URI          (URI)
import           LSP.Data.Range        (Range)
import           LSP.Data.Diagnostic   (Diagnostic)
import           LSP.Data.Registration (Registration)
import           Misc                  ((<|))
import qualified Misc

-- INITIALIZED --
initialized :: Text
initialized = "initialized"

-- TEXT DOCUMENT DID OPEN --
textDocumentDidOpen :: Text
textDocumentDidOpen = "textDocument/didOpen"

newtype TextDocumentDidOpenParams =
  TextDocumentDidOpenParams (URI, Int, Text)
  deriving (Show)

instance FromJSON TextDocumentDidOpenParams where
  parseJSON =
    A.withObject "TextDocumentDidOpenParams" <| \v ->
      v .: "textDocument" >>= \subV ->
        Misc.curryTriple TextDocumentDidOpenParams <$> subV .: "uri" <*>
        subV .: "version" <*>
        subV .: "text"

-- TEXT DOCUMENT DID CHANGE --
newtype ContentChange =
  ContentChange Text
  deriving (Show)

instance FromJSON ContentChange where
  parseJSON =
    A.withObject "ContentChange" <| \v -> fmap ContentChange (v .: "text")

textDocumentDidChange :: Text
textDocumentDidChange = "textDocument/didChange"

newtype TextDocumentDidChangeParams =
  TextDocumentDidChangeParams (URI, Int, [ContentChange])
  deriving (Show)

instance FromJSON TextDocumentDidChangeParams where
  parseJSON =
    A.withObject "TextDocumentDidChangeParams" <| \v ->
      let applied =
            v .: "textDocument" >>= \subV ->
              return (Misc.curryTriple TextDocumentDidChangeParams)
                <*> subV .: "uri"
                <*> subV .: "version"
      in
      applied
        <*> v .: "contentChanges"

-- TEXT DOCUMENT DID SAVE --
textDocumentDidSave :: Text
textDocumentDidSave = "textDocument/didSave"

newtype TextDocumentDidSaveParams =
  TextDocumentDidSaveParams URI
  deriving (Show)

instance FromJSON TextDocumentDidSaveParams where
  parseJSON =
    A.withObject "TextDocumentDidSaveParams" <| \v ->
      v .: "textDocument" >>= \subV ->
        return TextDocumentDidSaveParams
          <*> subV .: "uri"


-- PUBLISH DIAGNOSTICS --
publishDiagnostics:: Text
publishDiagnostics = "textDocument/publishDiagnostics"

newtype PublishDiagnosticsParams =
  PublishDiagnosticsParams (URI, [Diagnostic])
  deriving (Show)

instance FromJSON PublishDiagnosticsParams where
  parseJSON =
    A.withObject "PublishDiagnosticsParams" <| \v ->
      return (curry PublishDiagnosticsParams)
        <*> v .: "uri"
        <*> v .: "diagnostics"

instance ToJSON PublishDiagnosticsParams where
  toJSON (PublishDiagnosticsParams (uri, diagnostics)) =
    A.object
      [ "uri" .= uri
      , "diagnostics" .= diagnostics
      ]


-- REGISTER CAPABILITY --
registerCapability:: Text
registerCapability = "client/registerCapability"

newtype RegisterCapabilityParams =
  RegisterCapabilityParams [Registration]
  deriving (Show)

instance FromJSON RegisterCapabilityParams where
  parseJSON =
    A.withObject "RegisterCapabilityParams" <| \v ->
      return RegisterCapabilityParams
        <*> v .: "registrations"

instance ToJSON RegisterCapabilityParams where
  toJSON (RegisterCapabilityParams registrations) =
    A.object
      [ "registrations" .= registrations
      ]


-- DID CHANGE WATCHED FILES
didChangeWatchedFiles :: Text
didChangeWatchedFiles = "workspace/didChangeWatchedFiles"


newtype DidChangeWatchedFilesParams =
  DidChangeWatchedFilesParams [FileEvent]
  deriving (Show)

instance FromJSON DidChangeWatchedFilesParams where
  parseJSON =
    A.withObject "DidChangeWatchedFilesParams" <| \v ->
      return DidChangeWatchedFilesParams
        <*> v .: "changes"

instance ToJSON DidChangeWatchedFilesParams where
  toJSON (DidChangeWatchedFilesParams fileEvents) =
    A.object [ "changes" .= fileEvents ]


-- EXIT --
exit :: Text
exit = "exit"


-- METHODS --
data NotificationMethod
  = Initialized
  | TextDocumentDidOpen TextDocumentDidOpenParams
  | PublishDiagnostics PublishDiagnosticsParams
  | TextDocumentDidChange TextDocumentDidChangeParams
  | TextDocumentDidSave TextDocumentDidSaveParams
  | RegisterCapability RegisterCapabilityParams
  | DidChangeWatchedFiles DidChangeWatchedFilesParams
  | Exit
  deriving (Show)

decoder :: HM.HashMap Text Value -> Text -> Parser NotificationMethod
decoder v key
  | key == initialized = return Initialized
  | key == textDocumentDidOpen = TextDocumentDidOpen <$> v .: "params"
  | key == publishDiagnostics = PublishDiagnostics <$> v .: "params"
  | key == textDocumentDidChange = TextDocumentDidChange <$> v .: "params"
  | key == textDocumentDidSave = TextDocumentDidSave <$> v .: "params"
  | key == registerCapability = RegisterCapability <$> v .: "params"
  | key == didChangeWatchedFiles = DidChangeWatchedFiles <$> v .: "params"
  | key == exit = return Exit
  | otherwise = fail "Unknown notificaiton method"

instance FromJSON NotificationMethod where
  parseJSON =
    A.withObject "NotificationMethod" <| \v -> v .: "method" >>= decoder v

toPairs :: NotificationMethod -> [Pair]
toPairs message =
  case message of
    Initialized ->
      [ "method" .= initialized ]

    TextDocumentDidOpen _ ->
      [ "method" .= textDocumentDidOpen ]

    PublishDiagnostics params ->
      [ "method" .= publishDiagnostics
      , "params" .= params
      ]

    TextDocumentDidChange _ ->
      [ "method" .= textDocumentDidChange ]

    TextDocumentDidSave _ ->
      [ "method" .= textDocumentDidSave ]

    RegisterCapability params ->
      [ "method" .= registerCapability
      , "params" .= params
      ]

    DidChangeWatchedFiles params ->
      [ "method" .= registerCapability
      , "params" .= params
      ]

    Exit ->
      [ "method" .= exit ]
