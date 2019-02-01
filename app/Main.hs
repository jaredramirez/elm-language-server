module Main where

import qualified LSP.Server

main :: IO Int
main = LSP.Server.run
