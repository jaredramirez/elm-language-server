{-# LANGUAGE OverloadedStrings #-}

module LSP.MessageHandler
  ( handler
  ) where

import           Data.Aeson                  (Value)
import qualified Data.Aeson                  as A
import qualified Data.ByteString.Lazy        as BS
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.List                   as List
import qualified Data.Maybe                  as Maybe
import           LSP.Data.ElmConfig          (ElmConfig)
import qualified LSP.Data.ElmConfig          as ElmConfig
import qualified LSP.Data.Error              as Error
import           LSP.Data.Message            (Message)
import qualified LSP.Data.Message            as Message
import           LSP.Data.NotificationMethod ( TextDocumentDidOpenParams
                                             , TextDocumentDidChangeParams
                                             , TextDocumentDidSaveParams
                                             )
import qualified LSP.Data.NotificationMethod as NotificationMethod
import           LSP.Data.RequestMethod      (InitializeParams)
import qualified LSP.Data.RequestMethod      as RequestMethod
import qualified LSP.Data.URI                as URI
import           LSP.Data.Diagnostic         (Diagnostic)
import qualified LSP.Diagnostics             as Diagnostics
import qualified LSP.Log                     as Log
import qualified LSP.Misc
import           LSP.Model                   (Model)
import qualified LSP.Model                   as M
import           LSP.Update                  (Msg)
import qualified LSP.Update                  as U
import           Misc                        ((<|), (|>))
import qualified Misc
import qualified System.Directory            as Dir

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

    (True, Message.NotificationMessage (NotificationMethod.TextDocumentDidChange params)) ->
      textDocumentDidChangeHandler model params

    (True, Message.NotificationMessage (NotificationMethod.TextDocumentDidSave params)) ->
      textDocumentDidSaveHandler model params

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
    let
        (URI.URI projectRoot) =
          uri

        projectRootString =
          Text.unpack projectRoot

        pathToClonedRoot =
          projectRoot
            |> M.makeClonePathRoot

        elmConfigPath =
          projectRootString ++ "/elm.json"

        exectuableTask :: IO (Either Text FilePath)
        exectuableTask =
          projectRootString
            |> LSP.Misc.findElmExectuable

        elmConfigTask :: IO (Either Text ElmConfig)
        elmConfigTask =
            ElmConfig.parseFromFile elmConfigPath

        cloneElmSrcTask :: ElmConfig -> IO (Either Text ())
        cloneElmSrcTask elmConfig =
          let
              sourceDirectories =
                elmConfig
                  |> ElmConfig.getElmSourceDirectories
                  |> (\list ->
                        if List.null list then
                          ["."]

                        else
                          List.map (Text.unpack) list
                     )

              pathToClonedRootString =
                pathToClonedRoot |> Text.unpack
          in
          Dir.createDirectoryIfMissing True pathToClonedRootString >>
          Dir.copyFile elmConfigPath (pathToClonedRootString ++ "/elm.json") >>
          sequence (List.map (LSP.Misc.copyElmFileTree pathToClonedRootString) sourceDirectories) >>=
            \listOfEithers ->
              return
                (listOfEithers
                  |> sequence
                  |> fmap (\_ -> ())
                )

        eitherIOMsg =
          exectuableTask `andThenIO` \exectuable ->
          elmConfigTask  `andThenIO` \elmConfig ->
          cloneElmSrcTask elmConfig  `andThenIO` \_ ->
            U.Initialize
              id
              projectRoot
              (Text.pack exectuable)
              elmConfig
              pathToClonedRoot
              |> Right
              |> return
    in
    eitherIOMsg
      |> fmap
          (\either ->
            case either of
              Left errorMessage ->
                U.SendRequestError id Error.InternalError errorMessage

              Right msg ->
                msg
          )


textDocumentDidOpenHandler:: Model -> TextDocumentDidOpenParams -> IO Msg
textDocumentDidOpenHandler model (NotificationMethod.TextDocumentDidOpenParams (uri, version, document)) =
    let
        (URI.URI filePath) = uri

        eitherIOMsg =
          createFileCloneIfNeededTask model filePath `andThenIO` \createFilePath ->
          diagnosticsTask model createFilePath
    in
    eitherIOMsg
      |> fmap
        (\either ->
          case either of
            Left error ->
              U.SendNotifError Error.InternalError error

            Right diagnostics ->
              U.UpdateDocumentAndSendDiagnostics uri (M.Document version document) diagnostics
        )


textDocumentDidChangeHandler:: Model -> TextDocumentDidChangeParams -> IO Msg
textDocumentDidChangeHandler model (NotificationMethod.TextDocumentDidChangeParams (uri, version, contentChanges)) =
    let
        (URI.URI filePath) =
          uri

        lastContentChange =
          contentChanges
            |> List.reverse
            |> Misc.headSafe

        eitherIOMsg =
          case lastContentChange of
            Nothing ->
              return (Left "No document changes received")

            Just head ->
              let
                  (NotificationMethod.ContentChange actualContentChanges) =
                    head
              in
              createFileCloneIfNeededTask model filePath `andThenIO` \clonedFilePath ->
              updateFileContentsTask clonedFilePath actualContentChanges `andThenIO` \() ->
              diagnosticsTask model clonedFilePath `andThenIO` \diagnostics ->
                return (Right (diagnostics, actualContentChanges))
    in
    eitherIOMsg
      |> fmap
          (\either ->
            case either of
              Left errorMessage ->
                U.SendNotifError Error.InvalidParams errorMessage

              Right (diagnostics, contentChanges) ->
                U.UpdateDocumentAndSendDiagnostics
                  uri
                  (M.Document version contentChanges)
                  diagnostics
          )


textDocumentDidSaveHandler:: Model -> TextDocumentDidSaveParams -> IO Msg
textDocumentDidSaveHandler model (NotificationMethod.TextDocumentDidSaveParams uri) =
    let
        (URI.URI filePath) = uri
    in
    diagnosticsTask model filePath
      |> fmap
        (\elmMakeResult ->
          case elmMakeResult of
            Left error ->
              U.SendNotifError Error.InternalError error

            Right diagnostics ->
              U.SendDiagnostics uri diagnostics
        )


-- TASKS
diagnosticsTask :: Model -> Text -> IO (Either Text [Diagnostic])
diagnosticsTask model filePath =
  case M._package model of
    Nothing ->
      return (Left "Elm exectuable not found")

    Just (M.Package _ exectuable _ _) ->
      Diagnostics.run exectuable filePath


updateFileContentsTask :: Text -> Text -> IO (Either Text ())
updateFileContentsTask filePath nextContent =
  writeFile (Text.unpack filePath) (Text.unpack nextContent)
    |> LSP.Misc.ioToEither


createFileCloneIfNeededTask :: Model -> Text -> IO (Either Text Text)
createFileCloneIfNeededTask model fullFilePath =
  let
      maybeClonedFilePath =
        M.filePathToClonedFilePath model fullFilePath
  in
  case maybeClonedFilePath of
    Nothing ->
      return (Left "Cloned file root was not defined")

    Just clonedFilePath ->
      let
          fullFilePathString =
            Text.unpack fullFilePath

          clonedFilePathString =
            Text.unpack clonedFilePath

          parentDir =
            LSP.Misc.getFileParentDir clonedFilePathString
      in
      LSP.Misc.ioToEither (Dir.doesFileExist clonedFilePathString) `andThenIO`
        \doesExist ->
          if doesExist then
            return (Right clonedFilePath)

          else
            Dir.createDirectoryIfMissing True parentDir >>
            Dir.copyFile fullFilePathString clonedFilePathString >>
            return (Right clonedFilePath)

-- HELPERS
andThenIO :: IO (Either err resultA) -> (resultA -> IO (Either err resultB)) -> IO (Either err resultB)
andThenIO io func =
  io >>= \maybe ->
    case maybe of
      Left err ->
        return (Left err)

      Right value ->
        func value
