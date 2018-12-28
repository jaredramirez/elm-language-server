module Version
  ( asString
  ) where

import qualified Paths_elm_language_server
import qualified Data.Version


asString :: String
asString =
  Data.Version.showVersion Paths_elm_language_server.version
