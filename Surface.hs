{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Surface(
    Surface, X, Y,
    fromList, toList, (!), (!?), points, ffmap
    ) where

import Data.Array hiding ((!))
import qualified Data.Array as A
import qualified Data.Set as Set
import qualified Data.Map as Map


newtype X = X Int deriving (Eq,Ord,Ix,Enum)
newtype Y = Y Int deriving (Eq,Ord,Ix,Enum)


data Surface a = Surface (Array (X,Y) a)


fromList :: (Ord x, Ord y) => [(x,y,z)] -> Surface (x,y,Maybe z)
fromList vs = Surface $ array
    ((X 0, Y 0),(X (Map.size xs - 1), Y (Map.size ys - 1))) $
    [(g x y, (x,y,Nothing)) | x <- Map.keys xs, y <- Map.keys ys] ++ [(g x y, (x,y,Just z)) | (x,y,z) <- vs]
    where
        f :: Ord v => [v] -> Map.Map v Int
        f = Map.fromList . flip zip [0..] . Set.toList . Set.fromList
        g x y = (X $ xs Map.! x, Y $ ys Map.! y)
        xs = f [x | (x,_,_) <- vs]
        ys = f [y | (_,y,_) <- vs]

toList :: Surface x -> [x]
toList (Surface xs) = elems xs

(!) :: Surface a -> (X,Y) -> a
(!) (Surface xs) i = xs A.! i


(!?) :: Surface a -> (X,Y) -> Maybe a
(!?) (Surface xs) i
    | inRange (bounds xs) i = Just $ xs A.! i
    | otherwise = Nothing


points :: Surface a -> [(X,Y)]
points (Surface xs) = indices xs


ffmap :: (X -> Y -> a -> b) -> Surface a -> Surface b
ffmap f (Surface xs) = Surface $ array (bounds xs) [(xy, f (fst xy) (snd xy) v) | (xy,v) <- assocs xs]
