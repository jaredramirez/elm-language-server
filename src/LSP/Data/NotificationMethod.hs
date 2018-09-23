{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.NotificationMethod
  ( NotificationMethod(..)
  , TextDocumentDidOpenParams(..)
  ) where
import           Data.Aeson           (ToJSON, FromJSON, Value, (.:), (.=))
import qualified Data.Aeson           as A
import           Data.Aeson.Types     (Parser)
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict  as HM
import           Data.Text            (Text)
import           LSP.Data.URI         (URI)
import           LSP.Data.Range       (Range)
import           LSP.Data.Diagnostic  (Diagnostic)
import           Misc                 ((<|))
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

-- EXIT --
exit :: Text
exit = "exit"

-- METHODS --
data NotificationMethod
  = Initialized
  | TextDocumentDidOpen TextDocumentDidOpenParams
  | PublishDiagnostics PublishDiagnosticsParams
  | Exit
  deriving (Show)

decoder :: HM.HashMap Text Value -> Text -> Parser NotificationMethod
decoder v key
  | key == initialized = return Initialized
  | key == textDocumentDidOpen = TextDocumentDidOpen <$> v .: "params"
  | key == publishDiagnostics = PublishDiagnostics <$> v .: "params"
  | key == exit = return Exit
  | otherwise = fail "Unknown notificaiton method"

instance FromJSON NotificationMethod where
  parseJSON =
    A.withObject "NotificationMethod" <| \v -> v .: "method" >>= decoder v

instance ToJSON NotificationMethod where
  toJSON message =
    case message of
      Initialized ->
        A.object [ "method" .= initialized ]

      TextDocumentDidOpen _ ->
        A.object [ "method" .= textDocumentDidOpen ]

      PublishDiagnostics params ->
        A.object
          [ "method" .= textDocumentDidOpen
          , "params" .= params
          ]

      Exit ->
        A.object [ "method" .= exit ]
