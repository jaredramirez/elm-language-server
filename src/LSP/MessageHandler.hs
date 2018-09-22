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
import           Data.Semigroup              ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified LSP.Data.Error              as Error
import           LSP.Data.Message            (Message)
import qualified LSP.Data.Message            as Message
import           LSP.Data.NotificationMethod (TextDocumentDidOpenParams)
import qualified LSP.Data.NotificationMethod as NotificationMethod
import           LSP.Data.RequestMethod      (InitializeParams)
import qualified LSP.Data.RequestMethod      as RequestMethod
import qualified LSP.Data.URI                as URI
import qualified LSP.MessageHandler.Misc     as Misc
import           LSP.Model                   (Model)
import qualified LSP.Model                   as M
import           LSP.Update                  (Msg)
import qualified LSP.Update                  as U
import           Misc                        ((<|), (|>))

handler :: Model -> Message result -> IO Msg
handler model incomingMessage =
  case (M._initialized model, incomingMessage) of
    (False, Message.RequestMessage id (RequestMethod.Initialize paramsJson)) ->
      requestInitializeHandler id paramsJson

    (False, _) ->
      U.SendNotifError Error.ServerNotInitialized "Server Not Initialized"
        |> return

    (True, Message.NotificationMessage NotificationMethod.Initialized) ->
      U.NoOp
        |> return

    (True, Message.NotificationMessage (NotificationMethod.TextDocumentDidOpen paramsJson)) ->
      textDocumentDidOpenHandler paramsJson
        |> return

    (True, Message.RequestMessage _ RequestMethod.Shutdown) ->
      U.RequestShutDown
        |> return

    (True, Message.NotificationMessage NotificationMethod.Exit) ->
      U.Exit
        |> return

    (True, _) ->
      U.SendNotifError Error.MethodNotFound "Method not implemented"
        |> return

requestInitializeHandler:: Text -> Value -> IO Msg
requestInitializeHandler id paramsJson =
  let params :: A.Result InitializeParams
      params = A.fromJSON paramsJson
  in case params of
      A.Error error ->
        return (U.SendRequestError id Error.InvalidParams (Text.pack error))

      A.Success (RequestMethod.InitializeParams uri) ->
        let (URI.URI projectRoot) = uri
            exectuable = Misc.findElmExectuable (Text.unpack projectRoot)
        in exectuable >>= \case
            Left error ->
              return (U.SendRequestError id Error.InternalError error)

            Right executableValue ->
              return (U.Initialize id projectRoot (Text.pack executableValue))

textDocumentDidOpenHandler:: Value -> Msg
textDocumentDidOpenHandler paramsJson =
  let params :: A.Result TextDocumentDidOpenParams
      params = A.fromJSON paramsJson
  in case params of
      A.Error error ->
        U.SendNotifError Error.InvalidParams (Text.pack error)

      A.Success (NotificationMethod.TextDocumentDidOpenParams (uri, version, document)) ->
        U.SetDocument uri (M.Document version document)
