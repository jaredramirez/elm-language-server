{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module LSP.Misc
  ( ioToEither
  , findElmExectuable
  , getFileParentDir
  , copyElmFileTree
  , verifyElmVersion
  ) where

import           Control.Exception        (SomeException, tryJust)
import           Control.Monad.Trans      (liftIO)
import qualified Data.List                as List
import qualified Data.Maybe               as Maybe
import qualified Data.HashMap.Strict      as HM
import           Data.Semigroup           ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified LSP.Log                  as Log
import           Misc                     ((<|), (|>))
import qualified System.Directory         as Dir
import qualified System.FilePath          as FilePath
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
    liftIO <| Log.logger projectRoot
    let localPath = (Text.unpack projectRoot) ++ "/node_modules/.bin/elm"
    liftIO <| Log.logger localPath
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
getFileParentDir :: FilePath -> FilePath
getFileParentDir path =
  List.dropWhileEnd (\c -> c /= '/') path


extractDirectories :: [FilePath] -> [(FilePath, Bool)]
extractDirectories paths =
  paths
    |> List.foldl
        (\acc curPath ->
          let
              parentDir =
                getFileParentDir curPath
          in
          acc
            |> HM.insert parentDir True
            |> HM.insert curPath False
        )
        HM.empty
    |> HM.toList


getElmFiles :: FilePath -> IO [(FilePath, Bool)]
getElmFiles !source =
  source
    |> Glob.globDir1 (Glob.compile "**/*.elm")
    |> fmap
        (\filePaths ->
          filePaths
            |> List.filter
                (\item ->
                  -- TODO: do this in 1 pass with foldr
                  -- Not sure if lazyness takes care of this
                  not (List.isInfixOf "elm-stuff" item)
                    && not (List.isInfixOf "test" item)
                )
            |> List.map
                (\path ->
                  path
                    |> List.stripPrefix source
                    |> Maybe.fromMaybe path
                )
        )
    |> fmap extractDirectories


copyItem :: FilePath -> FilePath -> (FilePath, Bool) -> IO ()
copyItem !baseSourcePath !baseTargetPath (relativePath, isDir) =
    let
        sourcePath =
          baseSourcePath ++ relativePath

        targetPath =
          baseTargetPath ++ relativePath
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
