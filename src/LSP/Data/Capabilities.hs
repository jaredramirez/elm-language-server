{-# LANGUAGE OverloadedStrings #-}

module LSP.Data.Capabilities
  ( capabilities
  ) where

import           Data.Aeson           (Value)
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS

capabilities :: Value
capabilities =
  A.object
    [ ( "capabilities", A.object
        [ ("textDocumentSync", A.Number 1)
        , ("hoverProvider", A.Bool True)
        ]
      )
    ]
