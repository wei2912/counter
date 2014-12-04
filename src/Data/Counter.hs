module Data.Counter where

import qualified Data.Map.Strict as M
import qualified Data.List as L

{-|
  The Counter type is an alias
  of Data.Map.Strict.Map.
-}
type Counter k v = M.Map k v

{-|
  Initialize a counter with no keys.
-}
empty :: Counter k v
empty = M.empty

{-|
  Initialize a counter with a single
  instance of a key.
-}
singleton ::
	Num v
    => k -- ^ Key to be inserted.
    -> Counter k v -- ^ New counter.
singleton k = M.singleton k 1

{-|
  Increment the frequency count of a key
  with a specified value.
-}
updateWith ::
	(Ord k, Num v)
	=> k -- ^ Key to be inserted.
	-> v -- ^ Specified frequency count.
	-> Counter k v -- ^ Old counter.
	-> Counter k v -- ^ New counter.
updateWith = M.insertWith (+)

{-|
  Increment the frequency count of a key by 1.
-}
update ::
	(Ord k, Num v)
	=> k -- ^ Key to be inserted.
	-> Counter k v -- ^ Old counter.
	-> Counter k v -- ^ New counter.
update k = updateWith k 1

{-|
  Convert a list of items into a counter.
-}
count ::
	(Ord k, Num v)
	=> [k] -- ^ List of keys.
	-> Counter k v -- ^ New counter.
count = L.foldl' (flip update) M.empty

{-|
  Returns the union of two counters.
-}
union ::
	(Ord k, Num v)
	=> Counter k v -- ^ First counter.
	-> Counter k v -- ^ Second counter.
	-> Counter k v -- ^ Union of both counters.
union = M.unionWith (+)
