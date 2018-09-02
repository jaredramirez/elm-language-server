module Misc
  ( (|>)
  , (<|)
  , curryTriple
  , mapLeft
  , maybeToEither
  , andThen
  ) where

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

(<|) :: (a -> b) -> a -> b
(<|) = ($)

curryTriple :: ((a, b, c) -> d) -> a -> b -> c -> d
curryTriple f a b c = f (a, b, c)

mapLeft :: (a -> b) -> Either a e -> Either b e
mapLeft func either =
  case either of
    Left a  -> Left (func a)
    Right e -> Right e

maybeToEither :: error -> Maybe value -> Either error value
maybeToEither error maybe =
  case maybe of
    Nothing    -> Left error
    Just value -> Right value

andThen :: Monad m => (a -> m b) -> m a -> m b
andThen = (=<<)
