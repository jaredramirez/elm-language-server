{-# LANGUAGE OverloadedStrings #-}

module LSP.MessageHandler
  ( handler
  ) where

import qualified Analyze.Diagnostics         as Diagnostics
-- import qualified Analyze.Oracle              as Oracle
import qualified AST.Canonical               as Can
import           Control.Monad.Trans         (liftIO)
import qualified Elm.Compiler.Module         as Module
import           Elm.Project.Json            (Project)
import qualified Elm.Project.Json            as Project
import qualified Elm.Project.Summary         as Summary
import qualified LSP.Data.Error              as Error
import qualified Data.List                   as List
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
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
import qualified LSP.Log                     as Log
import qualified LSP.Misc
import           LSP.Model                   (Model)
import qualified LSP.Model                   as M
import           LSP.Update                  (Msg)
import qualified LSP.Update                  as U
import           Misc                        ((<|), (|>), andThen)
import qualified Misc
import qualified System.Directory            as Dir
import           System.FilePath             ((</>))
import qualified System.FilePath             as FilePath
import qualified Stuff.Verify                as Verify
import           Task                        (Task, SimpleTask)
import qualified Task


handler :: Model -> Message result -> SimpleTask Msg
handler model incomingMessage =
  case (M._initialized model, incomingMessage) of
    (False, Message.RequestMessage id (RequestMethod.Initialize params)) ->
      requestInitializeTask id params
        |> Task.mapError (\errorMessage -> U.SendRequestError id Error.InternalError errorMessage)

    (False, _) ->
      U.SendNotifError Error.ServerNotInitialized "Server Not Initialized"
        |> return

    (True, Message.NotificationMessage NotificationMethod.Initialized) ->
      U.NoOp
        |> return

    (True, Message.NotificationMessage (NotificationMethod.TextDocumentDidOpen params)) ->
      textDocumentDidOpenTask model params
        |> Task.mapError (\errorMessage -> U.SendNotifError Error.InternalError errorMessage)

    (True, Message.NotificationMessage (NotificationMethod.TextDocumentDidChange params)) ->
      textDocumentDidChangeTask model params
        |> Task.mapError (\errorMessage -> U.SendNotifError Error.InvalidParams errorMessage)

    (True, Message.NotificationMessage (NotificationMethod.TextDocumentDidSave params)) ->
      textDocumentDidSaveTask model params
        |> Task.mapError (\errorMessage -> U.SendNotifError Error.InternalError errorMessage)

    (True, Message.NotificationMessage (NotificationMethod.DidChangeWatchedFiles params)) ->
      didChangeWatchedFilesTask model params
        |> Task.mapError (\errorMessage -> U.SendNotifError Error.InternalError errorMessage)

    (True, Message.RequestMessage _id (RequestMethod.TextDocumentHover _params)) ->
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


requestInitializeTask:: Text -> InitializeParams -> Task Msg
requestInitializeTask id (RequestMethod.InitializeParams uri) =
  do
    let (URI.URI projectRoot) = uri
    let clonedProjectRoot = M.cloneProject projectRoot
    elmExectuable <- LSP.Misc.findElmExectuable projectRoot
    LSP.Misc.verifyElmVersion elmExectuable
    elmProject <- readAndCloneElmProject projectRoot clonedProjectRoot
    elmSummary <- Task.fromElmTask <| Verify.verify (Text.unpack projectRoot) elmProject
    cloneElmSrc elmProject clonedProjectRoot
    let foreignInterfaces = Summary._ifaces elmSummary
    let foreignImportDict = LSP.Misc.getForeignImportDict foreignInterfaces
    localInterfaces <- LSP.Misc.readInterfaces elmProject projectRoot
    return
      (U.Initialize id
        projectRoot
        clonedProjectRoot
        elmExectuable
        elmProject
        elmSummary
        foreignInterfaces
        foreignImportDict
        localInterfaces
      )


textDocumentDidOpenTask:: Model -> TextDocumentDidOpenParams -> Task Msg
textDocumentDidOpenTask model (NotificationMethod.TextDocumentDidOpenParams (uri, _version, source)) =
  do
    let (URI.URI filePath) = uri
    createFilePath <- createOrGetFileClone model filePath
    diagnostics <- getDiagnostics model createFilePath
    eitherCanonicalAndInterface <- getCanonicalAndInterface model source
    case eitherCanonicalAndInterface of
      Right (canonical, interface) ->
        return
          (U.UpdateModuleAndSendDiagnostics
            uri
            canonical
            (Can._name canonical)
            interface
            diagnostics
          )

      Left _ ->
        return (U.SendDiagnostics uri diagnostics)


textDocumentDidChangeTask:: Model -> TextDocumentDidChangeParams -> Task Msg
textDocumentDidChangeTask model (NotificationMethod.TextDocumentDidChangeParams (uri, _version, contentChanges)) =
  do
    let (URI.URI filePath) = uri
    let lastContentChange =
          contentChanges
            |> List.reverse
            |> Misc.headSafe
    case lastContentChange of
      Nothing ->
        Task.throw "No document changes received"

      Just (NotificationMethod.ContentChange source) ->
        do
          clonedFilePath <- createOrGetFileClone model filePath
          updateFileContents clonedFilePath source
          diagnostics <- getDiagnostics model clonedFilePath
          eitherCanonicalAndInterface <- getCanonicalAndInterface model source
          case eitherCanonicalAndInterface of
            Right (canonical, interface) ->
              return
                (U.UpdateModuleAndSendDiagnostics
                  uri
                  canonical
                  (Can._name canonical)
                  interface
                  diagnostics
                )

            Left _ ->
              return (U.SendDiagnostics uri diagnostics)


textDocumentDidSaveTask :: Model -> TextDocumentDidSaveParams -> Task Msg
textDocumentDidSaveTask model (NotificationMethod.TextDocumentDidSaveParams uri) =
  do
    let (URI.URI filePath) = uri
    diagnostics <- getDiagnostics model filePath
    return (U.SendDiagnostics uri diagnostics)


didChangeWatchedFilesTask :: Model -> DidChangeWatchedFilesParams -> Task Msg
didChangeWatchedFilesTask model (NotificationMethod.DidChangeWatchedFilesParams params) =
  do
    let relevantChange =
          List.foldl
            (\acc cur@(FileEvent.FileEvent uri changeType)  ->
              let
                  (URI.URI filePath) =
                    uri
              in
              if Text.isSuffixOf "elm.json" filePath then
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
    case (M._package model, relevantChange) of
      (Just package, Just _) ->
        do
          let projectRoot = package |> M._projectRoot
          elmProject <- readAndCloneElmProject projectRoot (M._clonedProjectRoot package)
          elmSummary <- Task.fromElmTask <| Verify.verify (projectRoot |> Text.unpack) elmProject
          return (U.UpdateElmProjectAndSummary elmProject elmSummary)

      (_, Nothing) ->
        Task.throw "No relevant changes"

      (Nothing, _) ->
        Task.throw "No existing elm data"


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
      -- todo: Search for reference with Oracle.hs
      U.SendRequestError id Error.InternalError "Package not initialized"



-- TASKS


-- This task clones the elm source to a cloned directory.
-- We do this so we can save chages to the clone and provide
-- as-you-type diagnostics to the user
cloneElmSrc :: Project -> Text -> Task ()
cloneElmSrc project clonedProjectRoot =
  let
      sourceDirectories =
        case project of
          Project.App (Project.AppInfo {Project._app_source_dirs = dirs}) ->
            dirs

          Project.Pkg (Project.PkgInfo {}) ->
            ["."]

      clonedProjectRootString =
        Text.unpack clonedProjectRoot
  in
  sourceDirectories
    |> List.map
        (\path ->
          do
            baseTargetPath <- liftIO <| Dir.makeAbsolute (clonedProjectRootString </> path)
            baseSourcePath <- liftIO <| Dir.makeAbsolute path
            LSP.Misc.copyElmFileTree baseTargetPath baseSourcePath
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
          let parentDir = FilePath.takeDirectory clonedFilePathString
          doesExist <- Task.lift <| Dir.doesFileExist clonedFilePathString
          if doesExist then
            return clonedFilePath

          else
            do
              liftIO <| Dir.createDirectoryIfMissing True parentDir
              liftIO <| Dir.copyFile fullFilePathString clonedFilePathString
              return clonedFilePath


-- Parse elm.json and clone it to our source clone
readAndCloneElmProject :: Text -> Text -> Task Project
readAndCloneElmProject projectRoot clonedRoot =
  do  let projectPath = M.elmProjectPath projectRoot
      let projectPathString = Text.unpack projectPath
      let clonedRootString = Text.unpack clonedRoot
      let clonedProjectPathString = Text.unpack (M.elmProjectPath clonedRoot)
      project <- Task.fromElmTask (Project.read projectPathString)
      liftIO <| Dir.createDirectoryIfMissing True clonedRootString
      liftIO <| Dir.copyFile projectPathString clonedProjectPathString
      return project



-- This is werid, but make a task always succeds
-- with it's success value an Either. We do this
-- because we don't want the failure to decode or
-- to getting an interface to fail this entire
-- message handler
getCanonicalAndInterface :: Model -> Text -> Task (Either Text (Can.Module, Module.Interface))
getCanonicalAndInterface model source =
  decodeModule model source
    |> andThen
        (\canonical ->
          LSP.Misc.getInterface canonical
            |> fmap (\interface -> (canonical, interface))
        )
    |> Task.try
    |> liftIO


-- Decode a module
decodeModule :: Model -> Text -> Task Can.Module
decodeModule model source =
  let
      maybePackage =
        model |> M._package
  in
  case maybePackage of
    Nothing ->
      Task.throw "No project"

    Just package ->
      let
          pkgName =
            package
              |> M._elmProject
              |> Project.getName
      in
      LSP.Misc.parseProgram pkgName source
        |> andThen
            (\valid ->
              LSP.Misc.canonicalize
                pkgName
                valid
                (M._foreignImportDict package)
                (M._localInterfaces package)
                (M._foreignInterfaces package)
            )
