{-# LANGUAGE OverloadedStrings #-}

module LSP.Model
  ( Model(..)
  , Package(..)
  , elmConfigFileName
  , toCloneProjectRoot
  , switchProjectRootWithClonedProjectRoot
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
  } deriving (Show)


data Package = Package
  { _projectRoot :: Text
  , _clonedProjectRoot :: Text
  , _exectuable :: Text
  , _elmConfig :: ElmConfig
  -- TODO: Add elm-format path
  } deriving (Show)


elmConfigFileName :: Text
elmConfigFileName =
  "elm.json"


toCloneProjectRoot :: Text -> Text
toCloneProjectRoot root =
  root <> "/elm-stuff/.lsp/clone"


switchProjectRootWithClonedProjectRoot :: Model -> Text -> Maybe Text
switchProjectRootWithClonedProjectRoot model filePath =
  _package model >>=
    \package ->
      let
          projectRoot =
            _projectRoot package

          clonedProjectroot =
            _clonedProjectRoot package
      in
      filePath
        |> Text.stripPrefix projectRoot
        |> fmap (\suffix -> clonedProjectroot <> suffix)
