{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module LSP.Misc
  ( ioToEither
  , findElmExectuable
  , copyElmFileTree
  , verifyElmVersion
  , readInterfaces
  , getForeignImportDict
  , getLocalImportDict
  , combineImportDicts
  ) where

import qualified AST.Source               as Src
import qualified AST.Valid                as AST
import           Control.Exception        (SomeException, tryJust)
import           Control.Monad.Trans      (liftIO)
import qualified Elm.Compiler.Module      as Module
import           Elm.Project.Json         (Project)
import qualified Elm.Project.Json         as Project
import           Elm.Package              as Package
import           Elm.Project.Json         as Project
import qualified Data.Binary              as Binary
import qualified Data.List                as List
import qualified Data.Maybe               as Maybe
import qualified Data.HashMap.Strict      as HM
import qualified Data.Map                 as Map
import           Data.Semigroup           ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified LSP.Log                  as Log
import qualified LSP.Model                as M
import           Misc                     ((<|), (|>))
import qualified Reporting.Annotation     as A
import qualified System.Directory         as Dir
import qualified System.FilePath          as FilePath
import           System.FilePath          ((</>))
import qualified System.FilePath.Glob     as Glob
import           Task                     (Task)
import qualified Task
import           System.Exit              as SysE
import           System.Process           as SysP


ioToEither :: IO value -> IO (Either Text value)
ioToEither io =
  tryJust exceptionToText io


exceptionToText :: SomeException -> Maybe Text
exceptionToText ex = Just (Text.pack (show ex))


-- ELM EXECTUABLE SEARCH --
findElmExectuable :: Text -> Task Text
findElmExectuable projectRoot =
  do
    let localPath = Text.unpack projectRoot ++ "/node_modules/.bin/elm"
    doesExist <- liftIO <| Dir.doesFileExist localPath
    if doesExist then
      localPath
        |> Text.pack
        |> return
    else
      do
        maybeExectuable <- liftIO <| Dir.findExecutable "elm"
        case maybeExectuable of
          Nothing ->
            Task.throw
              ("I couldn't find an elm executable!  I didn't see"
                <> " it in \"node_modules/.bin/\" or your $PATH."
              )

          Just executable ->
            return (Text.pack executable)


-- VERIFY ELM VERSION
verifyElmVersion :: Text -> Task ()
verifyElmVersion elmExectuablePath =
  do
    (exitCode, stdOutString, _stdErrString) <-
      liftIO <|
        SysP.readProcessWithExitCode
          (Text.unpack elmExectuablePath)
          ["--version"]
          ""
    case exitCode of
      SysE.ExitFailure _ ->
        Task.throw "Failed to read elm version"

      SysE.ExitSuccess ->
        case stdOutString of
          "0.19.0\n" ->
            return ()

          _ ->
            Task.throw "Invalid elm version"


-- COPY ELM FILE TREE
extractDirectories :: [FilePath] -> [(FilePath, Bool)]
extractDirectories paths =
  paths
    |> List.foldl
        (\acc curPath ->
          let
              parentDir =
                FilePath.takeDirectory curPath
          in
          acc
            |> HM.insert parentDir True
            |> HM.insert curPath False
        )
        HM.empty
    |> HM.toList


getSubElmProjects :: FilePath -> IO [FilePath]
getSubElmProjects source =
  do
    filePaths <- Glob.globDir1 (Glob.compile ("**/" ++ M.elmProject)) source
    filePaths
      |> List.map
          (\path ->
            -- We subtract the extra 1 to remove the trailing "/"
            List.take
              (List.length path - List.length M.elmProject - 1)
              path
          )
      |> List.filter
          (\path ->
            path /= source && not (M.elmStuff `List.isInfixOf` path)
          )
      |> return


isPrefixOfAny :: [FilePath] -> FilePath -> Bool
isPrefixOfAny listOfDirs filePath =
  List.foldr
    (\dir hasAlreadyFoundMatch ->
      if hasAlreadyFoundMatch then
        True

      else
        List.isPrefixOf dir filePath
    )
    False
    listOfDirs


getElmFiles :: FilePath -> IO [(FilePath, Bool)]
getElmFiles !source =
  do
    filePaths <- Glob.globDir1 (Glob.compile "**/*.elm") source
    subElmProjectPaths <- getSubElmProjects source
    filePaths
      |> List.filter
          (\item ->
            not (List.isInfixOf M.elmStuff item)
              && not (item |> isPrefixOfAny subElmProjectPaths)
          )
      |> List.map
          (\path ->
            path
              |> List.stripPrefix source
              |> fmap (List.drop 1)
              |> Maybe.fromMaybe path
          )
      |> extractDirectories
      |> return


copyItem :: FilePath -> FilePath -> (FilePath, Bool) -> IO ()
copyItem !baseSourcePath !baseTargetPath (relativePath, isDir) =
    let
        sourcePath =
          baseSourcePath </> relativePath

        targetPath =
          baseTargetPath </> relativePath
    in
    if isDir then
      Dir.createDirectoryIfMissing True targetPath

    else
      Dir.copyFile sourcePath targetPath


copyElmFileTreeHelper :: FilePath -> FilePath -> IO ()
copyElmFileTreeHelper !destination !source =
    Dir.createDirectoryIfMissing True destination >>
    getElmFiles source >>= \subItems ->
      subItems
        |> List.foldl
            (\(dirs, paths) cur@(_path, isDir) ->
              if isDir then
                (cur : dirs, paths)

              else
                (dirs, cur : paths)
            )
            ([], [])
        |> (\(dirs, paths) -> dirs ++ paths)
        |> mapM_ (copyItem source destination)


copyElmFileTree :: FilePath -> FilePath -> Task ()
copyElmFileTree destination source =
  Task.lift
    (copyElmFileTreeHelper
      (FilePath.normalise destination)
      (FilePath.normalise source)
    )


-- Read in *.elmi files as interfaces
readInterfaces :: Project -> Text -> Task Module.Interfaces
readInterfaces project projectRoot =
  let
      projectName =
        project
          |> Project.getName

      interfacesPath =
        projectRoot
          |> M.elmInterfacesPath
          |> Text.unpack
  in
  do
    filePaths <- liftIO <| Glob.globDir1 (Glob.compile "**/*.elmi") interfacesPath
    tuples <- mapM (readInterface projectName) filePaths
    tuples
      |> Map.fromList
      |> return


readInterface :: Package.Name -> FilePath -> Task (Module.Canonical, Module.Interface)
readInterface pkgName filePath =
  let
      maybeRawModuleName =
        filePath
          |> FilePath.takeBaseName
          |> Text.pack
          |> Module.fromHyphenPath
  in
  do
    doesExist <-liftIO (Dir.doesFileExist filePath)
    case (doesExist, maybeRawModuleName) of
      (True, Just rawModuleName) ->
        do
          decoded <- liftIO (Binary.decodeFile filePath)
          return (Module.Canonical pkgName rawModuleName, decoded)

      (False, _)->
        Task.throw ("File \"" <> Text.pack filePath <> "\" was not found")

      (_, Nothing)->
        Task.throw ("File \"" <> Text.pack filePath <> "\" didn't have a valid name")


-- Get imports for a module
getForeignImportDict :: Module.Interfaces -> M.ImportDict
getForeignImportDict foreignInterfaces =
  foreignInterfaces
    |> Map.toList
    |> List.map (\(canonical@(Module.Canonical _ name), _) -> (name, canonical))
    |> Map.fromList


getLocalImportDict :: Project -> AST.Module -> M.ImportDict
getLocalImportDict project localModule =
  let
      pkgName = Project.getName project
  in
  localModule
    |> AST._imports
    |> List.map
        (\(Src.Import (A.At _ importName) _ _) ->
          (importName, Module.Canonical pkgName importName)
        )
    |> Map.fromList


-- Note that `combineImportDicts` is left-biased, so any foreign modules imported
-- and created in `localImports` will be overridden by their actual canonical info
-- from `foreignImports`
combineImportDicts :: M.ImportDict -> M.ImportDict -> M.ImportDict
combineImportDicts foreignImports localImports =
  Map.union foreignImports localImports


-- todo: update interface
