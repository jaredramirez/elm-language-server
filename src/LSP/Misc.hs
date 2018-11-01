{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module LSP.Misc
  ( ioToEither
  , findElmExectuable
  , getFileParentDir
  , copyElmFileTree
  ) where

import           Control.Exception        (SomeException, catch, tryJust)
import           Data.List                ((\\))
import qualified Data.List                as List
import qualified Data.Maybe               as Maybe
import qualified Data.HashMap.Strict      as HM
import           Data.Semigroup           ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified LSP.Log                  as Log
import           Misc                     ((<|), (|>), mapLeft)
import qualified System.Directory         as Dir
import           System.FilePath          ((</>))
import qualified System.FilePath          as FilePath
import qualified System.FilePath.Glob     as Glob

ioToEither :: IO value -> IO (Either Text value)
ioToEither io =
  tryJust exceptionToText io

-- ELM EXECTUABLE SEARCH --
findElmExectuable :: FilePath -> IO (Either Text FilePath)
findElmExectuable elmFilePath =
  let dir = elmFilePath |> List.dropWhileEnd (/= '/') |> init
  in catch (findElmExectuableHelper dir) handleException


findElmExectuableHelper :: FilePath -> IO (Either Text FilePath)
findElmExectuableHelper !path =
  findInDir "node_modules" path >>= \case
    Nothing ->
      Dir.findExecutable "elm" >>= \maybeExectuable ->
        return
          (maybeToEither
             ("I couldn't find an elm executable!  I didn't see " <>
              "any \"node_modules\" folder to look for a local installation, " <>
              "so I only checked your $PATH for a global installation.")
             maybeExectuable)
    Just nodeModulesContents ->
      findInDir ".bin" nodeModulesContents >>= lift (findInDir "elm") >>= \case
        Nothing ->
          Dir.findExecutable "elm" >>= \maybeExectuable ->
            return
              (maybeToEither
                 ("Could not find elm executable! I searched \"" <>
                  Text.pack path <>
                  "/node_modules/.bin/\" for a local installation, " <>
                  "and your $PATH for a global installation.")
                 maybeExectuable)
        Just elmPath -> return (Right elmPath)


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
        (List.map
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
    putStrLn relativePath >>
    putStrLn relativePath >>
    putStrLn sourcePath >>
    putStrLn targetPath >>
    putStrLn "" >>
    if isDir then
      Dir.createDirectoryIfMissing True targetPath

    else
      Dir.copyFile sourcePath targetPath


copyElmFileTreeHelper :: FilePath -> FilePath -> IO ()
copyElmFileTreeHelper !destination !source =
    Dir.createDirectoryIfMissing True destination >>
    getElmFiles source >>= \subItems ->
      putStrLn (show subItems) >>
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


copyElmFileTree :: FilePath -> FilePath -> IO (Either Text ())
copyElmFileTree destination source =
  tryJust exceptionToText
    (copyElmFileTreeHelper
      (FilePath.normalise destination)
      (FilePath.normalise source)
    )


exceptionToText :: SomeException -> Maybe Text
exceptionToText ex = Just (Text.pack (show ex))


-- SEARCH HELPERS
findInDir :: String -> FilePath -> IO (Maybe FilePath)
findInDir !search !filePath =
  Dir.getDirectoryContents filePath >>= \filePathContents ->
    let searchResult = List.find (List.isSuffixOf search) filePathContents
    in case searchResult of
         Nothing     -> return Nothing
         Just result -> return (Just result)


lift :: (param -> IO (Maybe result)) -> Maybe param -> IO (Maybe result)
lift func param =
  case param of
    Nothing         -> return Nothing
    Just paramValue -> func paramValue


maybeToEither :: e -> Maybe r -> Either e r
maybeToEither error maybeResult =
  case maybeResult of
    Nothing     -> Left error
    Just result -> Right result


handleException :: SomeException -> IO (Either Text a)
handleException ex = return (Left (Text.pack (show ex)))
