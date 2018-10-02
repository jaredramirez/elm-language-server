{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module LSP.MessageHandler
  ( handler
  ) where

import           Data.Aeson                  (Value)
import qualified Data.Aeson                  as A
import           Control.Exception           ( SomeException
                                             , catch
                                             )
import qualified Data.ByteString.Lazy        as BS
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.List                   as List
import qualified LSP.Data.Error              as Error
import           LSP.Data.Message            (Message)
import qualified LSP.Data.Message            as Message
import           LSP.Data.NotificationMethod (TextDocumentDidOpenParams)
import qualified LSP.Data.NotificationMethod as NotificationMethod
import           LSP.Data.RequestMethod      (InitializeParams)
import qualified LSP.Data.RequestMethod      as RequestMethod
import qualified LSP.Data.URI                as URI
import qualified LSP.Diagnostics             as Diagnostics
import qualified LSP.Misc                    as Misc
import           LSP.Model                   (Model)
import qualified LSP.Model                   as M
import           LSP.Update                  (Msg)
import qualified LSP.Update                  as U
import           Misc                        ((<|), (|>))

handler :: Model -> Message result -> IO Msg
handler model incomingMessage =
  case (M._initialized model, incomingMessage) of
    (False, Message.RequestMessage id (RequestMethod.Initialize params)) ->
      requestInitializeHandler id params

    (False, _) ->
      U.SendNotifError Error.ServerNotInitialized "Server Not Initialized"
        |> return

    (True, Message.NotificationMessage NotificationMethod.Initialized) ->
      U.NoOp
        |> return

    (True, Message.NotificationMessage (NotificationMethod.TextDocumentDidOpen params)) ->
      textDocumentDidOpenHandler model params

    (True, Message.RequestMessage _ RequestMethod.Shutdown) ->
      U.RequestShutDown
        |> return

    (True, Message.NotificationMessage NotificationMethod.Exit) ->
      U.Exit
        |> return

    (True, _) ->
      U.SendNotifError Error.MethodNotFound "Method not implemented"
        |> return

requestInitializeHandler:: Text -> InitializeParams -> IO Msg
requestInitializeHandler id (RequestMethod.InitializeParams uri) =
    let (URI.URI projectRoot) = uri
        exectuable = Misc.findElmExectuable (Text.unpack projectRoot)
    in exectuable >>= \case
        Left error ->
          return (U.SendRequestError id Error.InternalError error)

        Right executableValue ->
          return (U.Initialize id projectRoot (Text.pack executableValue))

textDocumentDidOpenHandler:: Model -> TextDocumentDidOpenParams -> IO Msg
textDocumentDidOpenHandler model (NotificationMethod.TextDocumentDidOpenParams (uri, version, document)) =
    let (URI.URI filePath) = uri
        documents = M.Document version document
        diagnostics =
          case M._projectMeta model of
            Nothing ->
              return (Left "Elm exectuable not found")

            Just (_projectRoot, exectuable) ->
              Diagnostics.run exectuable filePath
     in
     fmap
       (\elmMakeResult ->
          case elmMakeResult of
            Left error ->
              U.SendNotifError Error.InternalError error

            Right maybeDiagnostic ->
              U.DidOpen uri (M.Document version document) maybeDiagnostic
        )
        diagnostics
