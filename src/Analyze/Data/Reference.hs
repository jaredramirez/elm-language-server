module Analyze.Data.Reference
  ( Reference(..)
  ) where


import Data.Text (Text)


data Reference =
  External
    { name :: Text
    , fullyQualifiedName :: Text
    , typeSignature :: Text
    , comment :: Text
    }
    deriving (Eq)
