{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.State
  ( State(..)
  ) where

import           Data.Text (Text)
import           System.IO (Handle)

data State = State
  { rootPath :: Text
  , rootUri  :: Text
  , logger   :: Handle
  }
