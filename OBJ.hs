{-# LANGUAGE RecordWildCards #-}

module OBJ(
    Vertex(..), size, unit, normal,
    Face(..), showOBJ
    ) where

import Numeric
import qualified Data.Map as Map


---------------------------------------------------------------------
-- VERTEX LIBRARY

data Vertex = Vertex {x, y, z :: Double} deriving (Show,Eq,Ord)

instance Num Vertex where
    fromInteger = lift0 . fromInteger
    signum = lift0 . signum . size
    abs = lift1 abs
    negate = lift1 negate
    (-) = lift2 (-)
    (+) = lift2 (+)
    (*) = lift2 (*)

instance Fractional Vertex where
    fromRational = lift0 . fromRational
    (/) = lift2 (/)
    recip = lift1 recip

lift0 :: Double -> Vertex
lift0 v = Vertex v v v

lift1 :: (Double -> Double) -> Vertex -> Vertex
lift1 f v = Vertex (g x) (g y) (g z)
    where g sel = f $ sel v

lift2 :: (Double -> Double -> Double) -> Vertex -> Vertex -> Vertex
lift2 f v1 v2 = Vertex (g x) (g y) (g z)
    where g sel = f (sel v1) (sel v2)

normal :: Vertex -> Vertex -> Vertex -> Vertex
normal (Vertex x1 y1 z1) (Vertex x2 y2 z2) (Vertex x3 y3 z3) = Vertex
    ((y2-y1)*(z3-z1) - (y3-y1)*(z2-z1))
    ((z2-z1)*(x3-x1) - (x2-x1)*(z3-z1))
    ((x2-x1)*(y3-y1) - (x3-x1)*(y2-y1))


size :: Vertex -> Double
size (Vertex x y z) = sqrt (sqr x + sqr y + sqr z)
    where sqr v = v * v

unit :: Vertex -> Vertex
unit v | n == 0 = v
       | otherwise = v / fromRational (toRational n)
    where n = size v


---------------------------------------------------------------------
-- OBJ LIBRARY

data Face = Face {verticies :: [Vertex], normals :: [Vertex]}

data S = S {vs :: Map.Map Vertex Int, vns :: Map.Map Vertex Int}

showOBJ :: [Face] -> [String]
showOBJ = showFaces $ S Map.empty Map.empty
    where
        showFaces s [] = []
        showFaces s (f:fs) = showFace s f $ \s -> showFaces s fs

        showFace s Face{..} k =
            showList showVertex s verticies $ \s ps ->
            showList showNormal s normals $ \s ns ->
            (unwords $ "f" : zipWith (++) ps (ns ++ repeat "")) : k s

        showList showOne s [] k = k s []
        showList showOne s (p:ps) k = showOne s p $ \s i -> showList showOne s ps $ \s is -> k s (i:is)

        showVertex s@S{..} v@Vertex{..} k
            | Just i <- Map.lookup v vs = k s $ show i
            | otherwise = let i = Map.size vs + 1 in
                          (unwords ["v",shw x,shw y,shw z,'#':show i]) :
                          k s{vs = Map.insert v i vs} (show i)

        showNormal s@S{..} vn@Vertex{..} k
            | Just i <- Map.lookup vn vns = k s $ "//" ++ show i
            | otherwise = let i = Map.size vns + 1 in
                          (unwords ["vn",shw x,shw y,shw z, '#':show i]) :
                          k s{vns = Map.insert vn i vns} ("//" ++ show i)


shw x = showFFloat Nothing x ""


