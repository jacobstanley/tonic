module Tonic.Utils where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

------------------------------------------------------------------------
-- Utils

setDifferenceL :: Ord a => Set a -> [a] -> Set a
setDifferenceL s xs = S.difference s (S.fromList xs)

setIntersectionL :: Ord a => Set a -> [a] -> Set a
setIntersectionL s xs = S.intersection s (S.fromList xs)

mapDifferenceL :: Ord k => Map k v -> [k] -> Map k v
mapDifferenceL m xs = M.difference m (M.fromList (map (\x -> (x, ())) xs))

mapDifferenceS :: Ord k => Map k v -> Set k -> Map k v
mapDifferenceS m s = M.difference m (M.fromSet (const ()) s)

mapIntersectionS :: Ord k => Map k v -> Set k -> Map k v
mapIntersectionS m s = M.intersection m (M.fromSet (const ()) s)

mapUnionR :: Ord k => Map k v -> Map k v -> Map k v
mapUnionR = M.unionWith (\_ x -> x)

unsafeLookup :: (Ord k, Show k, Show v) => String -> k -> Map k v -> v
unsafeLookup msg k kvs = M.findWithDefault (error msg') k kvs
  where
    msg' = msg ++ ": name not found: " ++ show k ++ " in " ++ show (M.toList kvs)
