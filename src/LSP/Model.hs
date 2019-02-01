{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module LSP.Model
  ( Model(..)
  , Package(..)
  , ImportDict
  , ASTs
  , elmProject
  , elmProjectPath
  , elmStuff
  , elmStuffPath
  , elmInterfacesPath
  , cloneProject
  , switchProjectRootWithClonedProjectRoot
  ) where

import qualified AST.Canonical as Can
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Semigroup ((<>))
import qualified Elm.Compiler.Module as Module
import qualified Elm.Name as N
import qualified Elm.Package as P
import Elm.Project.Json (Project)
import Elm.Project.Summary (Summary)
import LSP.Data.URI (URI)
import Misc ((|>))
import System.FilePath ((</>))
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
  , _elmProject :: Project
  , _elmSummary :: Summary
  , _foreignInterfaces :: Module.Interfaces
  , _foreignImportDict :: ImportDict
  , _localInterfaces :: Module.Interfaces
  , _asts :: ASTs
  -- TODO: Add elm-format path?
  }


type ImportDict =
  Map Module.Raw Module.Canonical


type ASTs =
  Map URI Can.Module


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


-- INSTANCES


instance Show Package where
  show (Package root cloneRoot exectuable _project _summary _fi _fid _li _asts) =
    "Package: {"
      ++ "_projectRoot = "
      ++ show root
      ++ " _clonedProjectRoot = "
      ++ show cloneRoot
      ++ " _exectuable = "
      ++ show exectuable
      ++ "}"


instance Show N.Name where
  show name =
    "Name: " ++ N.toString name


instance Show P.Name where
  show name =
    "Name: " ++ P.toString name


instance Show Module.Canonical where
  show (Module.Canonical pkgName name) =
    "Name: {_package = " ++ show pkgName ++ ", _module = " ++ show name ++ "}"
