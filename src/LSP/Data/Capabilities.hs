{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Capabilities
  ( capabilities
  ) where

import           Data.Aeson           (FromJSON, Value, (.:), (.:?))
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS

capabilities :: Value
capabilities =
  A.object
    [ ( "capabilities", A.object
        [ ("textDocumentSync", A.Number 1)
        ]
      )
    ]

encoded :: BS.ByteString
encoded = A.encode capabilities
