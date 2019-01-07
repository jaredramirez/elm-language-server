{-# LANGUAGE OverloadedStrings #-}

module LSP.Model
  ( Model(..)
  , Package(..)
  , Module
  , elmConfigFileName
  , toCloneProjectRoot
  , switchProjectRootWithClonedProjectRoot
  ) where

import Analyze.Data.Documentation (Documentation, ModuleName)
import Analyze.Data.ElmConfig (ElmConfig, ElmVersion)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Semigroup ((<>))
import LSP.Data.URI (URI)
import qualified AST.Source as Src
import Misc ((|>))
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
  , _exectuableVersion :: ElmVersion
  , _elmConfig :: ElmConfig
  , _ASTs :: HashMap URI Module
  , _documentation :: HashMap ModuleName Documentation
  -- TODO: Add elm-format path
  } deriving (Show)


type Module =
  Src.Module [Src.Decl]


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
