{-# LANGUAGE OverloadedStrings #-}

module LSP.Model
  ( Model(..)
  , Package(..)
  , Module
  , elmProject
  , elmProjectPath
  , elmStuff
  , elmStuffPath
  , cloneProject
  , switchProjectRootWithClonedProjectRoot
  ) where

import Analyze.Data.Documentation (Documentation)
import qualified AST.Module.Name as ModuleName
import AST.Valid (Module)
import Elm.Project.Json (Project)
import Elm.Project.Summary (Summary)
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Semigroup ((<>))
import LSP.Data.URI (URI)
import Misc ((|>))
import System.FilePath ((</>))
import Prelude hiding (init)


data Model = Model
  { _shouldTerminate :: Bool
  , _initialized :: Bool
  , _package :: Maybe Package
  }


data Package = Package
  { _projectRoot :: Text
  , _clonedProjectRoot :: Text
  , _exectuable :: Text
  , _elmProject :: Project
  , _elmSummary :: Summary
  , _ASTs :: HashMap URI Module
  , _documentation :: Map ModuleName.Canonical Documentation
  -- TODO: Add elm-format path
  }


elmProject :: String
elmProject = "elm.json"


elmProjectPath :: Text -> Text
elmProjectPath root =
  Text.pack (Text.unpack root </> elmProject)


elmStuff :: String
elmStuff = "elm-stuff"


elmStuffPath :: Text -> Text
elmStuffPath root =
  Text.pack (Text.unpack root </> elmStuff)


cloneProject :: Text -> Text
cloneProject root =
  Text.pack (Text.unpack root </> elmStuff </>".lsp/clone")


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
