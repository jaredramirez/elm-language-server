{-# LANGUAGE OverloadedStrings #-}

module LSP.Update
  ( init
  , Msg(..)
  , update
  , Response(..)
  , ShouldTermiate(..)
  ) where

import           Data.Aeson                  ((.=))
import qualified Data.Aeson                  as A
import qualified Data.ByteString.Lazy        as BS
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HM
import           Data.Semigroup              ((<>))
import           Data.Text                   (Text)
import qualified LSP.Data.Capabilities       as Capabilities
import           LSP.Data.Error              (Error)
import           LSP.Data.ElmConfig          (ElmConfig)
import qualified LSP.Data.ElmConfig          as ElmConfig
import qualified LSP.Data.FileChangeType     as FileChangeType
import qualified LSP.Data.FileSystemWatcher  as FileSystemWatcher
import           LSP.Data.Message            (Message)
import qualified LSP.Data.Message            as Message
import           LSP.Data.Registration       (Registration)
import qualified LSP.Data.Registration       as Registration
import           LSP.Data.Diagnostic         (Diagnostic)
import qualified LSP.Data.Diagnostic         as Diagnostic
import           LSP.Data.MessageError       (MessageError)
import qualified LSP.Data.MessageError       as MessageError
import qualified LSP.Data.NotificationMethod as NotifMethod
import           LSP.Data.URI                (URI)
import qualified LSP.Data.URI                as URI
import           LSP.Model                   (Model)
import qualified LSP.Model                   as M
import           Misc                        ((<|), (|>))
import           Prelude                     hiding (init)

init :: Model
init = M.Model False False Nothing HM.empty

data Response
  = Send BS.ByteString
  | SendMany [BS.ByteString]
  | None
  deriving (Show)

data ShouldTermiate
  = ShouldTerminate
  | ShouldNotTerminate
  deriving (Show)

data Msg
  = Initialize Text Text Text ElmConfig Text
  | UpdateElmConfig ElmConfig
  | UpdateDocument URI M.Document
  | SendDiagnostics URI [Diagnostic]
  | UpdateDocumentAndSendDiagnostics URI M.Document [Diagnostic]
  | RequestShutDown
  | Exit
  | SendRequestError Text Error Text
  | SendNotifError Error Text
  | NoOp
  deriving (Show)

update :: Msg -> Model -> (Model, Response, ShouldTermiate)
update msg model =
  case msg of
    Initialize id projectRoot executable config clonedFilePath ->
      ( model
          { M._initialized = True
          , M._package =
            Just <|
              M.Package
                projectRoot
                executable
                config
                clonedFilePath
          }
      , SendMany
        [ Message.encode
            (Message.ResponseMessage
                (Just id)
                (Just Capabilities.capabilities)
                Nothing
            )
        , [ Registration.DidChangeWatchedFiles
              "watching"
              [ FileSystemWatcher.FileSystemWatcher
                  (projectRoot <> "/" <> M.elmConfigFileName)
                  (Just FileChangeType.Changed)
              ]
          ]
          |> NotifMethod.RegisterCapabilityParams
          |> NotifMethod.RegisterCapability
          |> Message.NotificationMessage
          |> (Message.encode :: Message () -> BS.ByteString)
        ]
      , ShouldNotTerminate
      )

    UpdateElmConfig elmConfig ->
      ( model
          { M._package =
              model
                |> M._package
                |> fmap (\package -> package { M._elmConfig = elmConfig })
          }
      , None
      , ShouldNotTerminate
      )

    UpdateDocument uri document ->
      ( model
          { M._documents =
              model
                |> M._documents
                |> HM.alter (const (Just document)) uri
          }
      , None
      , ShouldNotTerminate
      )

    SendDiagnostics uri diagnostics ->
      ( model
      , let encode :: Message () -> BS.ByteString
            encode = Message.encode
          in
          (uri, diagnostics)
            |> NotifMethod.PublishDiagnosticsParams
            |> NotifMethod.PublishDiagnostics
            |> Message.NotificationMessage
            |> encode
            |> Send
      , ShouldNotTerminate
      )

    UpdateDocumentAndSendDiagnostics uri document diagnostics ->
      ( model
          { M._documents =
              model
                |> M._documents
                |> HM.alter (const (Just document)) uri
          }
      , let encode :: Message () -> BS.ByteString
            encode = Message.encode
          in
          (uri, diagnostics)
            |> NotifMethod.PublishDiagnosticsParams
            |> NotifMethod.PublishDiagnostics
            |> Message.NotificationMessage
            |> encode
            |> Send
      , ShouldNotTerminate
      )

    RequestShutDown ->
      ( model { M._shouldTerminate = True }
      , None
      , ShouldNotTerminate
      )

    Exit ->
      (model, None, ShouldTerminate)

    SendRequestError id error errorMessage ->
      let messageError = MessageError.MessageError error errorMessage
          message :: Message ()
          message =
            Message.ResponseMessage (Just id) Nothing (Just messageError)
      in
        ( model
        , Send (Message.encode message)
        , ShouldNotTerminate
        )

    SendNotifError error errorMessage ->
      let messageError = MessageError.MessageError error errorMessage
          message :: Message ()
          message =
            Message.ResponseMessage Nothing Nothing (Just messageError)
      in
        ( model
        , Send (Message.encode message)
        , ShouldNotTerminate
        )

    NoOp ->
      ( model
      , None
      , ShouldNotTerminate
      )
