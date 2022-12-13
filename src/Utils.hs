module Utils
  ((<.>)
  , nTimes
  , indexedFind
  , within
  , ffmap
  ) where
import Data.Foldable (find)

infixl 4 <.>
(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f <.> g = fmap f . g

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f = foldr (.) id $ replicate n f

indexedFind :: (a -> Bool) -> [a] -> Maybe (Int, a)
indexedFind p = find (p . snd) . zip [0..]

within :: Ord a => a -> (a, a) -> Bool
within x (a, b) = x >= a && x <= b

ffmap :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
ffmap = fmap . fmap
