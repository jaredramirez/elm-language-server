{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Capabilities
  ( capabilities
  ) where

import           Data.Aeson           (FromJSON, Value, (.:), (.:?))
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS

capabilities :: Value
capabilities =
  A.object [("capabilities", A.object [("hoverProvider", A.Bool True)])]

encoded :: BS.ByteString
encoded = A.encode capabilities
