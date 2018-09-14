module Main where

import qualified Control.Monad
import qualified LSP.Server

main :: IO Int
main = LSP.Server.run
