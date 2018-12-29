{-# LANGUAGE OverloadedStrings #-}

module LSP.MessageHandler
  ( handler
  ) where

import qualified Analyze.Diagnostics         as Diagnostics
import           Data.Semigroup              ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.List                   as List
import           LSP.Data.ElmConfig          (ElmVersion, ElmConfig)
import qualified LSP.Data.ElmConfig          as ElmConfig
import qualified LSP.Data.Error              as Error
import qualified LSP.Data.FileEvent          as FileEvent
import qualified LSP.Data.FileChangeType     as FileChangeType
import           LSP.Data.Message            (Message)
import qualified LSP.Data.Message            as Message
import           LSP.Data.NotificationMethod ( TextDocumentDidOpenParams
                                             , TextDocumentDidChangeParams
                                             , TextDocumentDidSaveParams
                                             , DidChangeWatchedFilesParams
                                             )
import qualified LSP.Data.NotificationMethod as NotificationMethod
import           LSP.Data.RequestMethod      (InitializeParams)
import qualified LSP.Data.RequestMethod      as RequestMethod
import qualified LSP.Data.URI                as URI
import           LSP.Data.Diagnostic         (Diagnostic)
import qualified LSP.Misc
import           LSP.Model                   (Model)
import qualified LSP.Model                   as M
import           LSP.Update                  (Msg)
import qualified LSP.Update                  as U
import           Misc                        ((|>))
import qualified Misc
import qualified Parse.Parse                 as P
import qualified Reporting.Result            as R
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

    (True, Message.NotificationMessage (NotificationMethod.DidChangeWatchedFiles params)) ->
      didChangeWatchedFiles model params

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

        clonedProjectRoot =
          projectRoot |> M.toCloneProjectRoot

        getExectuableTask :: IO (Either Text Text)
        getExectuableTask =
          projectRoot |> LSP.Misc.findElmExectuable

        getElmVersionTask :: Text -> IO (Either Text ElmVersion)
        getElmVersionTask =
          LSP.Misc.getElmVersion

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
                          list
                     )
          in
          sequence
            (List.map
              (\path ->
                LSP.Misc.copyElmFileTree
                  (clonedProjectRoot <> "/" <> path)
                  path
              )
              sourceDirectories
            ) >>=
            \listOfEithers ->
              return
                (listOfEithers
                  |> sequence
                  |> fmap (\_ -> ())
                )

        eitherIOMsg =
          getExectuableTask `bindEitherIO` \exectuable ->
          getElmVersionTask exectuable `bindEitherIO` \exectuableVersion ->
            case exectuableVersion of
              ElmConfig.InvalidVersion ->
                U.InvalidElmVersion id
                  |> Right
                  |> return

              ElmConfig.V0_19 ->
                elmConfigTask projectRoot clonedProjectRoot `bindEitherIO` \elmConfig ->
                cloneElmSrcTask elmConfig `bindEitherIO` \_ ->
                  U.Initialize id
                    projectRoot
                    clonedProjectRoot
                    exectuable
                    exectuableVersion
                    elmConfig
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
        (URI.URI filePath) =
          uri

        maybeModule =
          document
            |> Text.unpack
            |> P.parseModule
            |> R.toMaybe

        eitherIOMsg =
          createOrGetFileCloneTask model filePath `bindEitherIO` \createFilePath ->
          diagnosticsTask model createFilePath
    in
    eitherIOMsg
      |> fmap
        (\either ->
          case either of
            Left error ->
              U.SendNotifError Error.InternalError error

            Right diagnostics ->
              U.SetASTAndSendDiagnostics uri maybeModule diagnostics
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

            Just contentChanges ->
              let
                  (NotificationMethod.ContentChange actualContentChanges) =
                    contentChanges

                  maybeModule =
                    actualContentChanges
                      |> Text.unpack
                      |> P.parseModule
                      |> R.toMaybe

              in
              createOrGetFileCloneTask model filePath `bindEitherIO` \clonedFilePath ->
              updateFileContentsTask clonedFilePath actualContentChanges `bindEitherIO` \() ->
              diagnosticsTask model clonedFilePath `bindEitherIO` \diagnostics ->
                return (Right (maybeModule, diagnostics))
    in
    eitherIOMsg
      |> fmap
          (\either ->
            case either of
              Left errorMessage ->
                U.SendNotifError Error.InvalidParams errorMessage

              Right (maybeModule, diagnostics) ->
                U.SetASTAndSendDiagnostics uri maybeModule diagnostics
          )


textDocumentDidSaveHandler :: Model -> TextDocumentDidSaveParams -> IO Msg
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


didChangeWatchedFiles :: Model -> DidChangeWatchedFilesParams -> IO Msg
didChangeWatchedFiles model (NotificationMethod.DidChangeWatchedFilesParams params) =
  let
      relevantChange =
        List.foldl
          (\acc cur@(FileEvent.FileEvent uri changeType)  ->
            let
                (URI.URI filePath) = uri
            in
            if Text.isSuffixOf M.elmConfigFileName filePath then
              case changeType of
                FileChangeType.Changed ->
                  Just cur

                _ ->
                  acc
            else
              acc
          )
          Nothing
          params

      task :: IO (Either Text ElmConfig)
      task =
        case (M._package model, relevantChange) of
          (Just package, Just _) ->
            elmConfigTask (M._projectRoot package) (M._clonedProjectRoot package)

          (_, Nothing) ->
            return (Left "No relevant changes")

          (Nothing, _) ->
            return (Left "No existing elm data")

  in
  fmap
    (\either ->
      case either of
        Left error ->
          U.SendNotifError Error.InternalError error

        Right elmConfig ->
          U.UpdateElmConfig elmConfig
    )
    task


-- TASKS

-- This task runs the elm compiler with the flag `--report=json` on
-- the given file and parses the results
diagnosticsTask :: Model -> Text -> IO (Either Text [Diagnostic])
diagnosticsTask model filePath =
  case M._package model of
    Nothing ->
      return (Left "Elm exectuable was not found")

    Just package ->
      Diagnostics.run (M._exectuable package) filePath


-- This task writes the given changes (it expects the whole file) to the
-- give path
updateFileContentsTask :: Text -> Text -> IO (Either Text ())
updateFileContentsTask filePath nextContent =
  writeFile (Text.unpack filePath) (Text.unpack nextContent)
    |> LSP.Misc.ioToEither


-- This task takes the filePath given, and clones it (and it's parent directories)
-- to the source clone
createOrGetFileCloneTask :: Model -> Text -> IO (Either Text Text)
createOrGetFileCloneTask model fullFilePath =
  let
      maybeClonedFilePath =
        M.switchProjectRootWithClonedProjectRoot model fullFilePath
  in
  case maybeClonedFilePath of
    Nothing ->
      return (Left "Issue getting cloned file path")

    Just clonedFilePath ->
      let
          fullFilePathString =
            Text.unpack fullFilePath

          clonedFilePathString =
            Text.unpack clonedFilePath

          parentDir =
            LSP.Misc.getFileParentDir clonedFilePathString
      in
      LSP.Misc.ioToEither (Dir.doesFileExist clonedFilePathString) `bindEitherIO`
        \doesExist ->
          if doesExist then
            return (Right clonedFilePath)

          else
            Dir.createDirectoryIfMissing True parentDir >>
            Dir.copyFile fullFilePathString clonedFilePathString >>
            return (Right clonedFilePath)


-- This task parses elm.json and clones it to our source clone
elmConfigTask :: Text -> Text -> IO (Either Text ElmConfig)
elmConfigTask projectRoot clonedRoot =
  let
      elmConfigPath =
        (projectRoot <> "/" <> M.elmConfigFileName)

      elmConfigPathString =
        Text.unpack elmConfigPath

      clonedRootString =
        Text.unpack clonedRoot

      clonedElmConfigPathString =
        clonedRootString ++ Text.unpack ("/" <> M.elmConfigFileName)
  in
  ElmConfig.parseFromFile elmConfigPath `bindEitherIO` \elmConfig ->
    Dir.createDirectoryIfMissing True clonedRootString >>
    Dir.copyFile elmConfigPathString clonedElmConfigPathString >>
    return (Right elmConfig)


-- HELPERS
bindEitherIO :: IO (Either err resultA) -> (resultA -> IO (Either err resultB)) -> IO (Either err resultB)
bindEitherIO io func =
  io >>= \either ->
    case either of
      Left err ->
        return (Left err)

      Right value ->
        func value
