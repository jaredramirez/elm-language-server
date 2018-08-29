module Misc
  ( (|>)
  , (<|)
  ) where

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

(<|) :: (a -> b) -> a -> b
(<|) = ($)
