{-# LANGUAGE OverloadedStrings #-}

module Analyze.Data.Documentation
  (
  ) where


import Prelude hiding (Left, Right)
import Data.Vector ((!?))
import Data.Aeson (FromJSON, (.:))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson as A
import Data.Text (Text)


data Documentation
  = Documentation
    { _name :: Text
    , _comment :: Text
    , _unions :: [Union]
    , _aliases :: [Alias]
    , _values :: [Value]
    , _binops :: [Binop]
    }

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


instance FromJSON UnionCase where
  parseJSON =
    A.withArray "UnionCase" $ \a ->
      case (a !? 0, a !? 2) of
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


instance FromJSON Associativity where
  parseJSON =
    A.withText "Associativity" $ \v ->
      if v == "right" then
        return Right

      else if v == "left" then
        return Left

      else
        fail "invalid Associativity"
