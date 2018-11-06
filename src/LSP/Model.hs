{-# LANGUAGE OverloadedStrings #-}

module LSP.Model
  ( Model(..)
  , Package(..)
  , Document(..)
  , elmConfigFileName
  , makeClonePathRoot
  , filePathToClonedFilePath
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Semigroup ((<>))
import LSP.Data.ElmConfig (ElmConfig)
import LSP.Data.URI (URI)
import Misc ((<|), (|>))
import Prelude hiding (init)


data Model = Model
  { _shouldTerminate :: Bool
  , _initialized :: Bool
  , _package :: Maybe Package
  , _documents :: HashMap URI Document
  } deriving (Show)


data Package = Package
  { _rootPath :: Text
  , _exectuable :: Text
  , _elmConfig :: ElmConfig
  , _clonedFilePathRoot :: Text
  -- TODO: Add elm-format path
  } deriving (Show)


data Document = Document
  { _version :: Int
  , _text :: Text
  } deriving (Show)


elmConfigFileName :: Text
elmConfigFileName =
  "elm.json"

makeClonePathRoot :: Text -> Text
makeClonePathRoot root =
  root <> "/elm-stuff/.lsp/clone"


filePathToClonedFilePath :: Model -> Text -> Maybe Text
filePathToClonedFilePath model filePath =
  _package model >>=
    \package ->
      let
          rootPath =
            _rootPath package

          clonedFilePathRoot =
            _clonedFilePathRoot package
      in
      filePath
        |> Text.stripPrefix rootPath
        |> fmap (\suffix -> clonedFilePathRoot <> suffix)
