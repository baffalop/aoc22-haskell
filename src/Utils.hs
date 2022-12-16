module Utils
  ((<.>)
  , nTimes
  , indexedFind
  , within
  , ffmap
  , partitionWith
  , pairs
  , manhattanDistance
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

partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith f = flip foldr ([], []) \x (lefts, rights) ->
  case f x of
    Left left -> (left:lefts, rights)
    Right right -> (lefts, right:rights)

pairs :: [a] -> [(a, a)]
pairs = zip <$> id <*> drop 1

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (y1, x1) (y2, x2) = abs (y2 - y1) + abs (x2 - x1)
