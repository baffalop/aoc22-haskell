module Utils
  ((<.>)
  ) where

infixl 4 <.>
(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f <.> g = fmap f . g
