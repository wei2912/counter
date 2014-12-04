module Data.Counter.Frac where

import qualified Data.Map.Strict as M
import qualified Data.List as L

type Counter k v = M.Map k v

-- Initialize a counter with no keys.
empty :: Counter k v
empty = M.empty

-- Initialize a counter with a single
-- instance of a key.
singleton :: Num v => k -> Counter k v
singleton k = M.singleton k 1

-- Increment the frequency count of a key
-- with a specified value.
updateWith :: (Ord k, Num v) => k -> v -> Counter k v -> Counter k v
updateWith = M.insertWith (+)

-- Increment the frequency count of a key by 1.
increment :: (Ord k, Num v) => k -> Counter k v -> Counter k v
increment k = updateWith k 1

-- Convert a list of items into a counter.
count :: (Ord k, Num v) => [k] -> Counter k v
count = L.foldl' (flip increment) M.empty

-- Returns the union of two counters.
union :: (Ord k, Num v) => Counter k v -> Counter k v -> Counter k v
union = M.unionWith (+)
