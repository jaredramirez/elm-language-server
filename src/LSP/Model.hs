module LSP.Model
  ( Model(..)
  , Package(..)
  , Document(..)
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import LSP.Data.ElmConfig (ElmConfig)
import LSP.Data.URI (URI)
import Prelude hiding (init)

data Model = Model
  { _shouldTerminate :: Bool
  , _initialized :: Bool
  , _projectMeta :: Maybe (Text, Text)
  , _documents :: HashMap URI Document
  , _projects :: HashMap URI Package
  } deriving (Show)

data Package = Package
  { _rootPath :: Text
  , _exectuable :: Text
  , _elmConfig :: ElmConfig
  , _localModules :: [Text]
  , _hiddenBuildPath :: Text
  -- TODO: Add elm-format path
  } deriving (Show)

data Document = Document
  { _version :: Int
  , _text :: Text
  } deriving (Show)
