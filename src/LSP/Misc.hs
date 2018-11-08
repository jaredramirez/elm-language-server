{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module LSP.Misc
  ( ioToEither
  , findElmExectuable
  , getFileParentDir
  , copyElmFileTree
  , getElmVersion
  ) where

import           Control.Exception        (SomeException, catch, tryJust)
import           Data.List                ((\\))
import qualified Data.List                as List
import qualified Data.Maybe               as Maybe
import qualified Data.HashMap.Strict      as HM
import           Data.Semigroup           ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           LSP.Data.ElmConfig       (ElmVersion)
import qualified LSP.Data.ElmConfig       as ElmConfig
import qualified LSP.Log                  as Log
import           Misc                     ((<|), (|>), mapLeft)
import qualified System.Directory         as Dir
import           System.FilePath          ((</>))
import qualified System.FilePath          as FilePath
import qualified System.FilePath.Glob     as Glob
import           System.Exit              as SysE
import           System.Process           as SysP

ioToEither :: IO value -> IO (Either Text value)
ioToEither io =
  tryJust exceptionToText io


-- ELM EXECTUABLE VERSION --
getElmVersion :: Text -> IO (Either Text ElmVersion)
getElmVersion elmExectuablePath =
  fmap
    (\(exitCode, stdOutString, stdErrString) ->
      case exitCode of
        SysE.ExitFailure _ ->
          Left "Failed to get version"

        SysE.ExitSuccess ->
          case stdOutString of
            "0.19.0\n" ->
              Right ElmConfig.V0_19

            _ ->
              Right ElmConfig.InvalidVersion
    )
    (SysP.readProcessWithExitCode
      (Text.unpack elmExectuablePath)
      ["--version"]
      ""
    )


-- ELM EXECTUABLE SEARCH --
findElmExectuable :: Text -> IO (Either Text Text)
findElmExectuable projectRoot =
  let normalised = projectRoot |> Text.unpack |> FilePath.normalise
  in catch (findElmExectuableHelper normalised) handleExceptionEither


findElmExectuableHelper :: FilePath -> IO (Either Text Text)
findElmExectuableHelper !path =
  let
      localPath = path ++ "/node_modules/.bin/elm"
  in
  Log.logger path >>
  Log.logger localPath >>
  Dir.doesFileExist localPath >>= \doesExist ->
    if doesExist then
      localPath
        |> Text.pack
        |> Right
        |> return

    else
      fmap
        (\maybeExectuable ->
          maybeExectuable
            |> fmap Text.pack
            |> maybeToEither
              ("I couldn't find an elm executable!  I didn't see"
                <> " it in \"node_modules/.bin/\" or your $PATH."
              )
        )
        (Dir.findExecutable "elm")


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
            |> List.filter (not . List.isInfixOf "elm-stuff")
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
            (\(dirs, paths) cur@(path, isDir) ->
              if isDir then
                (cur : dirs, paths)

              else
                (dirs, cur : paths)
            )
            ([], [])
        |> (\(dirs, paths) -> dirs ++ paths)
        |> mapM_ (copyItem source destination)


copyElmFileTree :: Text -> Text -> IO (Either Text ())
copyElmFileTree destination source =
  tryJust exceptionToText
    (copyElmFileTreeHelper
      (destination |> Text.unpack |> FilePath.normalise)
      (source |> Text.unpack |> FilePath.normalise)
    )


exceptionToText :: SomeException -> Maybe Text
exceptionToText ex = Just (Text.pack (show ex))


-- SEARCH HELPERS
maybeToEither :: e -> Maybe r -> Either e r
maybeToEither error maybeResult =
  case maybeResult of
    Nothing ->
      Left error

    Just result ->
      Right result


handleExceptionEither :: SomeException -> IO (Either Text a)
handleExceptionEither ex =
  return (Left (Text.pack (show ex)))


handleExceptionMaybe :: SomeException -> IO (Maybe a)
handleExceptionMaybe ex =
  return Nothing
