{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Analyze.Data.Documentation
  ( ModuleName(..)
  , Documentation(..)
  , Union(..)
  , UnionCase(..)
  , Alias(..)
  , Value(..)
  , Binop(..)
  , readDocumentationFromDependencies
  ) where


import Analyze.Data.ElmConfig (ExactVersion, DependencyName)
import qualified Analyze.Data.ElmConfig as ElmConfig
import Data.Aeson (FromJSON, FromJSONKey, (.:))
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import Data.Hashable as H
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Traversable as Traversable
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector ((!?))
import Prelude hiding (Left, Right)
import System.Directory as SysDir
import Misc ((|>))
import qualified Misc


newtype ModuleName =
  ModuleName { _moduleNameText :: Text }
  deriving (Show, Eq, Ord, H.Hashable, FromJSONKey)


instance FromJSON ModuleName where
  parseJSON =
    A.withText "ModuleName" (\text -> return (ModuleName text))


data Documentation
  = Documentation
    { _name :: ModuleName
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


readDocumentationFromDependencies ::
  HashMap DependencyName ExactVersion
  -> IO (Either Text (HashMap ModuleName Documentation))
readDocumentationFromDependencies dependencies = do
  homeDirectory <- SysDir.getHomeDirectory
  listOfEithers <-
    dependencies
      |> HM.toList
      |> List.map
          (\((ElmConfig.DependencyName name), (ElmConfig.Exact version)) ->
            (homeDirectory
              ++ "/.elm/0.19.0/package/"
              ++ Text.unpack name
              ++ "/"
              ++ Text.unpack version
              ++ "/"
              ++ "documentation.json"
            )
              |> BS.readFile
              |> fmap
                (A.eitherDecode'
                  :: BS.ByteString -> Either String [Documentation]
                )
          )
      |> Traversable.sequence
  listOfEithers
    |> Traversable.sequence
    |> Misc.mapLeft Text.pack
    |> fmap
      (\listOfDocumentations ->
        listOfDocumentations
          |> List.concat
          |> fmap (\documentation -> (_name documentation, documentation))
          |> HM.fromList
      )
    |> return
