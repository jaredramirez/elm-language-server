{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module LSP.MessageHandler.Misc
  ( findElmExectuable
  ) where

import           Control.Exception        (SomeException, catch)
import qualified Data.List                as List
import           Data.Semigroup           ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Misc                     ((<|), (|>))
import qualified System.Directory         as Dir


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
