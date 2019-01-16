{-# LANGUAGE OverloadedStrings #-}

module LSP.MessageHandler
  ( handler
  ) where

import qualified Analyze.Diagnostics         as Diagnostics
import qualified Analyze.Data.Documentation  as Documentation
-- import qualified Analyze.Oracle              as Oracle
import AST.Valid                             (Module)
import           Control.Monad.Trans         (liftIO)
import           Elm.Project.Json            (Project)
import qualified Elm.Project.Json            as Project
import qualified LSP.Data.Error              as Error
import qualified Data.List                   as List
import           Data.Semigroup              ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as TextEncode
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
import qualified LSP.Data.Position           as Position
import           LSP.Data.RequestMethod      (InitializeParams)
import qualified LSP.Data.RequestMethod      as RequestMethod
import qualified LSP.Data.URI                as URI
import           LSP.Data.Diagnostic         (Diagnostic)
import qualified LSP.Misc
import           LSP.Model                   (Model)
import qualified LSP.Model                   as M
import           LSP.Update                  (Msg)
import qualified LSP.Update                  as U
import           Misc                        ((<|), (|>))
import qualified Misc
import qualified Parse.Parse                 as Parse
import qualified System.Directory            as Dir
import qualified Reporting.Result            as ElmResult
import qualified Result
import           Task                        (Task)
import qualified Task


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

    (True, Message.RequestMessage id (RequestMethod.TextDocumentHover params)) ->
      U.RequestShutDown
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


requestInitializeHandler:: Text -> InitializeParams -> IO Msg
requestInitializeHandler id (RequestMethod.InitializeParams uri) =
  let
      task =
        do
          let (URI.URI projectRoot) = uri
          let clonedProjectRoot = projectRoot |> M.toCloneProjectRoot
          elmExectuable <- LSP.Misc.findElmExectuable projectRoot
          LSP.Misc.verifyElmVersion elmExectuable
          elmProject <- readElmProject projectRoot clonedProjectRoot
          cloneElmSrc elmProject clonedProjectRoot
          -- TODO: Maybe remove reading documentation? Canonizalization may remove
          -- the need
          documentationMap <- Documentation.readDocumentationFromProject elmProject
          return
            (U.Initialize id
              projectRoot
              clonedProjectRoot
              elmExectuable
              elmProject
              documentationMap
            )
    in
    Task.run (\errorMessage -> U.SendRequestError id Error.InternalError errorMessage)
      task


textDocumentDidOpenHandler:: Model -> TextDocumentDidOpenParams -> IO Msg
textDocumentDidOpenHandler model (NotificationMethod.TextDocumentDidOpenParams (uri, _version, document)) =
    let
        task =
          do
            let (URI.URI filePath) = uri
            let maybeModule = decodeModule model document
            createFilePath <- createOrGetFileClone model filePath
            diagnostics <- getDiagnostics model createFilePath
            return (U.SetASTAndSendDiagnostics uri maybeModule diagnostics)
    in
    Task.run (\errorMessage -> U.SendNotifError Error.InternalError errorMessage)
      task


textDocumentDidChangeHandler:: Model -> TextDocumentDidChangeParams -> IO Msg
textDocumentDidChangeHandler model (NotificationMethod.TextDocumentDidChangeParams (uri, _version, contentChanges)) =
    let
        task =
            do
              let (URI.URI filePath) = uri
              let lastContentChange =
                    contentChanges
                      |> List.reverse
                      |> Misc.headSafe
              case lastContentChange of
                Nothing ->
                  Task.throw "No document changes received"

                Just (NotificationMethod.ContentChange actualContentChanges) ->
                  do
                    let maybeModule = decodeModule model actualContentChanges
                    clonedFilePath <- createOrGetFileClone model filePath
                    updateFileContents clonedFilePath actualContentChanges
                    diagnostics <- getDiagnostics model clonedFilePath
                    return (U.SetASTAndSendDiagnostics uri maybeModule diagnostics)
    in
    Task.run (\errorMessage -> U.SendNotifError Error.InvalidParams errorMessage)
      task


textDocumentDidSaveHandler :: Model -> TextDocumentDidSaveParams -> IO Msg
textDocumentDidSaveHandler model (NotificationMethod.TextDocumentDidSaveParams uri) =
  let
      task =
        do
          let (URI.URI filePath) = uri
          diagnostics <- getDiagnostics model filePath
          return (U.SendDiagnostics uri diagnostics)
  in
  Task.run (\errorMessage -> U.SendNotifError Error.InternalError errorMessage)
    task


didChangeWatchedFiles :: Model -> DidChangeWatchedFilesParams -> IO Msg
didChangeWatchedFiles model (NotificationMethod.DidChangeWatchedFilesParams params) =
  let
      task =
        do
          let relevantChange =
                List.foldl
                  (\acc cur@(FileEvent.FileEvent uri changeType)  ->
                    let
                        (URI.URI filePath) =
                          uri
                    in
                    if Text.isSuffixOf M.elmProjectFileName filePath then
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
          elmProject <-
            case (M._package model, relevantChange) of
              (Just package, Just _) ->
                readElmProject (M._projectRoot package) (M._clonedProjectRoot package)

              (_, Nothing) ->
                Task.throw "No relevant changes"

              (Nothing, _) ->
                Task.throw "No existing elm data"
          return (U.UpdateElmProject elmProject)
  in
  Task.run (\errorMessage -> U.SendNotifError Error.InternalError errorMessage)
    task


hover :: Text -> Model -> RequestMethod.TextDocumentHoverParams -> Msg
hover id model (RequestMethod.TextDocumentHoverParams (uri, position)) =
  let
      (URI.URI filePath) =
        uri

      maybeClonedFilePath =
        M.switchProjectRootWithClonedProjectRoot
          model
          filePath

      (Position.Position (line, character)) =
        position
  in
  case maybeClonedFilePath of
    Nothing ->
      U.SendRequestError id Error.InternalError "Package not initialized"

    Just clonedFilePath ->
      -- TODO: Search for reference with Oracle.hs
      U.SendRequestError id Error.InternalError "Package not initialized"



-- TASKS


-- This task clones the elm source to a cloned directory.
-- We do this so we can save chages to the clone and provide
-- as-you-type diagnostics to the user
cloneElmSrc :: Project -> Text -> Task ()
cloneElmSrc project clonedProjectRoot =
  do
    let sourceDirectories =
          case project of
            Project.App (Project.AppInfo {Project._app_source_dirs = dirs}) ->
              dirs

            Project.Pkg (Project.PkgInfo {}) ->
              ["."]
    let clonedProjectRootString =
          Text.unpack clonedProjectRoot ++ "/"
    sourceDirectories
      |> List.map
          (\path ->
            LSP.Misc.copyElmFileTree
              (clonedProjectRootString ++ path)
              path
          )
      |> sequence_


-- This task runs the elm compiler with the flag `--report=json` on
-- the given file and parses the results
getDiagnostics :: Model -> Text -> Task [Diagnostic]
getDiagnostics model filePath =
  case M._package model of
    Nothing ->
      Task.throw "Elm exectuable was not found"

    Just package ->
      Diagnostics.run (M._exectuable package) filePath


-- This task writes the given changes (it expects the whole file) to the
-- give path
updateFileContents :: Text -> Text -> Task ()
updateFileContents filePath nextContent =
  writeFile (Text.unpack filePath) (Text.unpack nextContent)
    |> Task.lift


-- This task takes the filePath given, and clones it (and it's parent directories)
-- to the source clone
createOrGetFileClone :: Model -> Text -> Task Text
createOrGetFileClone model fullFilePath =
  do
    let maybeClonedFilePath = M.switchProjectRootWithClonedProjectRoot model fullFilePath
    case maybeClonedFilePath of
      Nothing ->
        Task.throw "Issue getting cloned file path"

      Just clonedFilePath ->
        do
          let fullFilePathString = Text.unpack fullFilePath
          let clonedFilePathString = Text.unpack clonedFilePath
          let parentDir = LSP.Misc.getFileParentDir clonedFilePathString
          doesExist <- Task.lift <| Dir.doesFileExist clonedFilePathString
          if doesExist then
            return clonedFilePath

          else
            do
              liftIO <| Dir.createDirectoryIfMissing True parentDir
              liftIO <| Dir.copyFile fullFilePathString clonedFilePathString
              return clonedFilePath


-- This task parses elm.json and clones it to our source clone
readElmProject :: Text -> Text -> Task Project
readElmProject projectRoot clonedRoot =
  do  let projectPath = (projectRoot <> "/" <> M.elmProjectFileName)
      let projectPathString = Text.unpack projectPath
      let clonedRootString = Text.unpack clonedRoot
      let clonedProjectPathString = clonedRootString ++ Text.unpack ("/" <> M.elmProjectFileName)
      project <- Task.fromElmTask (Project.read projectPathString)
      liftIO <| Dir.createDirectoryIfMissing True clonedRootString
      liftIO <| Dir.copyFile projectPathString clonedProjectPathString
      return project


-- Decode a module
decodeModule :: Model -> Text -> Maybe Module
decodeModule model document =
  let
      maybeProjectName =
        model
          |> M._package
          |> fmap
              (\package ->
                package
                  |> M._elmProject
                  |> Project.getName
              )
  in
  maybeProjectName
    |> Misc.andThen
        (\projectName ->
          document
            |> TextEncode.encodeUtf8
            |> Parse.program projectName
            |> Misc.andThen ElmResult.ok
            |> Result.fromElmResult
        )
