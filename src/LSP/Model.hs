{-# LANGUAGE OverloadedStrings #-}

module LSP.Model
  ( Model(..)
  , Package(..)
  , ImportDict
  , elmProject
  , elmProjectPath
  , elmStuff
  , elmStuffPath
  , elmInterfacesPath
  , cloneProject
  , switchProjectRootWithClonedProjectRoot
  ) where

import qualified Elm.Compiler.Module as Module
import Elm.Project.Json (Project)
import Elm.Project.Summary (Summary)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Semigroup ((<>))
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
  , _foreignInterfaces :: Module.Interfaces
  , _foreignImportDict :: ImportDict
  , _localInterfaces :: Module.Interfaces
  -- TODO: Add elm-format path?
  }


type ImportDict =
  Map Module.Raw Module.Canonical


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


elmInterfacesPath :: Text -> Text
elmInterfacesPath root =
  Text.pack (Text.unpack root </> elmStuff </> "0.19.0")


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
