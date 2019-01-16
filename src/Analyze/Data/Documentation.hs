{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Analyze.Data.Documentation
  ( Documentation(..)
  , Union(..)
  , UnionCase(..)
  , Alias(..)
  , Value(..)
  , Binop(..)
  , readDocumentationFromProject
  ) where


import qualified AST.Module.Name as ModuleName
import Control.Monad.Trans (liftIO)
import Data.Aeson (FromJSON, (.:))
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Traversable as Traversable
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector ((!?))
import qualified Elm.Name as N
import Elm.Package (Name, Version)
import qualified Elm.Package as Package
import qualified Elm.Project.Constraint as Con
import Elm.Project.Json (Project)
import qualified Elm.Project.Json as Project
import Misc ((<|), (|>), andThen)
import qualified Misc
import Prelude hiding (Left, Right)
import System.Directory as SysDir
import Task (Task)
import qualified Task



data Documentation
  = Documentation
    { _name :: Text
    , _comment :: Text
    , _unions :: [Union]
    , _aliases :: [Alias]
    , _values :: [Value]
    , _binops :: [Binop]
    }
    deriving (Show)

instance FromJSON Documentation where
  parseJSON =
    A.withObject "Documentation" $ \v ->
      return Documentation
        <*> v .: "name"
        <*> v .: "comment"
        <*> v .: "unions"
        <*> v .: "aliases"
        <*> v .: "values"
        <*> v .: "binops"


data Union =
  Union
    { _unionName :: Text
    , _unionComment :: Text
    , _unionArgs :: [Text]
    , _cases :: [UnionCase]
    }
    deriving (Show)


instance FromJSON Union where
  parseJSON =
    A.withObject "Union" $ \v ->
      return Union
        <*> v .: "name"
        <*> v .: "comment"
        <*> v .: "args"
        <*> v .: "cases"


data UnionCase =
  UnionCase Text [Text]
  deriving (Show)


instance FromJSON UnionCase where
  parseJSON =
    A.withArray "UnionCase" $ \a ->
      case (a !? 0, a !? 1) of
        (Just nameValue, Just argsValue) ->
          return UnionCase
            <*> (A.parseJSON nameValue :: Parser Text)
            <*> (A.parseJSON argsValue :: Parser [Text])

        _ ->
          fail "invalid UnionCase"


data Alias =
  Alias
    { _aliasName :: Text
    , _aliasComment :: Text
    , _aliasArgs :: [Text]
    , _aliasType :: Text
    }
    deriving (Show)


instance FromJSON Alias where
  parseJSON =
    A.withObject "Alias" $ \v ->
      return Alias
        <*> v .: "name"
        <*> v .: "comment"
        <*> v .: "args"
        <*> v .: "type"


data Value =
  Value
    { _valueName :: Text
    , _valueComment :: Text
    , _valueType :: Text
    }
    deriving (Show)


instance FromJSON Value where
  parseJSON =
    A.withObject "Value" $ \v ->
      return Value
        <*> v .: "name"
        <*> v .: "comment"
        <*> v .: "type"


data Binop =
  Binop
    { _binopName :: Text
    , _binopComment :: Text
    , _binopType :: Text
    , _binopAssociativity :: Associativity
    , _binopPrecedence :: Int
    }
    deriving (Show)


instance FromJSON Binop where
  parseJSON =
    A.withObject "Binop" $ \v ->
      return Binop
        <*> v .: "name"
        <*> v .: "comment"
        <*> v .: "type"
        <*> v .: "associativity"
        <*> v .: "precedence"


data Associativity
  = Right
  | Left
  | None
  deriving (Show)


instance FromJSON Associativity where
  parseJSON =
    A.withText "Associativity" $ \v ->
      if v == "right" then
        return Right

      else if v == "left" then
        return Left

      else if v == "non" then
        return None

      else
        fail "invalid Associativity"


getLowestMatchingVersion :: FilePath -> Name -> Con.Constraint -> Task Version
getLowestMatchingVersion packageDir name constraint =
  do
    let packageFilePath = Package.toFilePath name
    let dir = packageDir ++ packageFilePath
    contents <- liftIO <| SysDir.getDirectoryContents dir
    let maybeVersion =
          contents
            |> List.foldr
                (\versionString acc ->
                  case acc of
                    Nothing ->
                      Package.versionFromText (Text.pack versionString)
                        |> andThen
                            (\version ->
                              if Con.satisfies constraint version then
                                Just version

                              else
                                Nothing
                            )

                    Just _ ->
                      acc
                )
                Nothing
    case maybeVersion of
      Nothing ->
        Task.throw ("No matching version for package " <> Text.pack (Package.toFilePath name))

      Just version ->
        return version


readDocumentationFromProject :: Project -> Task (Map ModuleName.Canonical Documentation)
readDocumentationFromProject project =
  do
    homeDirectory <- liftIO <| SysDir.getHomeDirectory
    let packageDir = homeDirectory ++ "/.elm/0.19.0/package/"

    -- Read package/versions from project
    pkgVersionMap <-
      case project of
        Project.App (Project.AppInfo {Project._app_deps_direct = deps}) ->
          return deps

        Project.Pkg (Project.PkgInfo {Project._pkg_deps = deps}) ->
          deps
            |> Map.mapWithKey (getLowestMatchingVersion packageDir)
            |> Traversable.sequence

    -- Read the documentation file for each package/version
    -- moduleEitherDocMap :: Map Name (Either String [Documentation])’
    moduleEitherDocMap <-
      pkgVersionMap
        |> Map.mapWithKey
            (\name version ->
              (packageDir
                ++ Package.toFilePath name
                ++ "/"
                ++ Package.versionToString version
                ++ "/"
                ++ "documentation.json"
              )
                |> BS.readFile
                |> fmap
                  (A.eitherDecode' :: BS.ByteString -> Either String [Documentation])
            )
        |> Traversable.sequence
        |> liftIO

    -- Sequence the results and transform the error type
    -- moduleDocMap :: Either Text (Map Name Documentation)’
    moduleDocMap <-
      moduleEitherDocMap
        |> Traversable.sequence
        |> Misc.mapLeft Text.pack
        |> Task.liftEither

    -- Change from (Map PackageName [Documentation]) to (Map Module Documentation)
    -- for easier lookups
    moduleDocMap
      |> Map.toList
      |> List.foldr
          (\(pkgName, documentations) acc ->
            List.map
              (\documentation ->
                (ModuleName.Canonical
                  pkgName
                  (documentation
                    |> _name
                    |> N.fromText
                  )
                , documentation
                )
              )
              documentations
              ++ acc
          )
          []
      |> Map.fromList
      |> return
