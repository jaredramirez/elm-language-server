module Main where

import qualified Control.Monad
import qualified LSP.Server

main :: IO ()
main = Control.Monad.void LSP.Server.run
