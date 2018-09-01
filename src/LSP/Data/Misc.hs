module LSP.Data.Misc
  ( curryTriple
  ) where

curryTriple :: ((a, b, c) -> d) -> a -> b -> c -> d
curryTriple f a b c = f (a, b, c)
