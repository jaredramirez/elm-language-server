{-# LANGUAGE OverloadedStrings #-}

module LSP.Update
  ( init
  , Msg(..)
  , update
  , Response(..)
  , ShouldTermiate(..)
  ) where

import           Analyze.Data.Documentation  (Documentation)
import qualified AST.Module.Name             as ModuleName
import qualified Data.ByteString.Lazy        as BS
import qualified Data.HashMap.Strict         as HM
import           Data.Semigroup              ((<>))
import           Data.Map                    (Map)
import           Data.Text                   (Text)
import           Elm.Project.Json            (Project)
import qualified LSP.Data.Capabilities       as Capabilities
import           LSP.Data.Error              (Error)
import qualified LSP.Data.Error              as Error
import qualified LSP.Data.FileSystemWatcher  as FileSystemWatcher
import           LSP.Data.Message            (Message)
import qualified LSP.Data.Message            as Message
import qualified LSP.Data.Registration       as Registration
import           LSP.Data.Diagnostic         (Diagnostic)
import qualified LSP.Data.MessageError       as MessageError
import qualified LSP.Data.NotificationMethod as NotifMethod
import qualified LSP.Data.FileChangeType     as FileChangeType
import           LSP.Data.URI                (URI)
import           LSP.Model                   (Model, Module)
import qualified LSP.Model                   as M
import           Misc                        ((<|), (|>))
import           Prelude                     hiding (init)

init :: Model
init = M.Model False False Nothing

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
  = Initialize Text Text Text Text Project (Map ModuleName.Canonical Documentation)
  | SetASTAndSendDiagnostics URI (Maybe Module) [Diagnostic]
  | SendDiagnostics URI [Diagnostic]
  | UpdateElmProject Project
  | RequestShutDown
  | Exit
  | SendRequestError Text Error Text
  | SendNotifError Error Text
  | InvalidElmVersion Text
  | NoOp



update :: Msg -> Model -> (Model, Response, ShouldTermiate)
update msg model =
  case msg of
    Initialize id projectRoot clonedProjectRoot executable project docs ->
      ( model
          { M._initialized = True
          , M._package =
            Just <|
              M.Package projectRoot
                clonedProjectRoot
                executable
                project
                HM.empty
                docs
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
                  (projectRoot <> "/" <> M.elmProjectFileName)
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

    SetASTAndSendDiagnostics uri maybeAST diagnostics ->
      ( model
        { M._package =
            model
              |> M._package
              |> fmap
                (\package ->
                  package
                    { M._ASTs =
                      package
                        |> M._ASTs
                        |> HM.alter
                          (\maybeExisting ->
                            case maybeAST of
                              Nothing ->
                                maybeExisting

                              Just ast ->
                                Just ast
                          )
                          uri
                    }
                )
        }
      , (uri, diagnostics)
          |> encodeDiagnostics
          |> Send
      , ShouldNotTerminate
      )

    SendDiagnostics uri diagnostics ->
      ( model
      , (uri, diagnostics)
          |> encodeDiagnostics
          |> Send
      , ShouldNotTerminate
      )

    UpdateElmProject elmProject ->
      ( model
          { M._package =
              model
                |> M._package
                |> fmap (\package -> package { M._elmProject = elmProject })
              }
      , None
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

    InvalidElmVersion id ->
      ( model
      , SendMany
        [ NotifMethod.ShowMessageParams NotifMethod.Info
          "elm-language-server is only compatible with Elm v0.19.0 and greater"
          |> NotifMethod.ShowMessage
          |> Message.NotificationMessage
          |> (Message.encode :: Message () -> BS.ByteString)
        , let
              messageError =
                MessageError.MessageError
                  Error.UnknownErrorCode
                  "elm-language-server is only compatible with Elm v0.19.0 and greater"
          in
          Message.ResponseMessage (Just id) Nothing
            (Just messageError)
            |> (Message.encode :: Message () -> BS.ByteString)
        ]
      , ShouldTerminate
      )

    NoOp ->
      ( model
      , None
      , ShouldNotTerminate
      )

encodeDiagnostics :: (URI, [Diagnostic]) -> BS.ByteString
encodeDiagnostics tuple =
    let encode :: Message () -> BS.ByteString
        encode = Message.encode
    in
    tuple
      |> NotifMethod.PublishDiagnosticsParams
      |> NotifMethod.PublishDiagnostics
      |> Message.NotificationMessage
      |> encode


instance Show Msg where
  show msg =
    case msg of
      Initialize {} ->
        "Initialize"

      SetASTAndSendDiagnostics {} ->
        "SetASTAndSendDiagnostics"

      SendDiagnostics {} ->
        "SendDiagnostics"

      RequestShutDown ->
        "RequestShutDown"

      Exit ->
        "Exit"

      SendRequestError _ _ _ ->
        "SendRequestError"

      SendNotifError _ _ ->
        "SendNotifError"

      InvalidElmVersion _ ->
        "InvalidElmVersion"

      NoOp ->
        "NoOp"
