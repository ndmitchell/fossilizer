{-# LANGUAGE RecordWildCards #-}

module OBJ(
    Vertex(..), Vector,
    Face(..), showOBJ,
    normal, unit, follow
    ) where

import Numeric
import qualified Data.Map as Map


data Vertex = Vertex {x, y, z :: Double} deriving (Eq,Ord)

type Vector = Vertex


data Face = Face {verticies :: [Vertex], normals :: [Vector]}

data S = S {vs :: Map.Map Vertex Int, vns :: Map.Map Vector Int}

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
                          (unwords ["v",shw x,shw y,shw z]) :
                          k s{vs = Map.insert v i vs} (show i)

        showNormal s@S{..} vn@Vertex{..} k
            | Just i <- Map.lookup vn vns = k s $ "//" ++ show i
            | otherwise = let i = Map.size vns + 1 in
                          (unwords ["vn",shw x,shw y,shw z]) :
                          k s{vns = Map.insert vn i vns} ("//" ++ show i)


shw x = showFFloat Nothing x ""


normal :: Vertex -> Vertex -> Vertex -> Vertex
normal (Vertex x1 y1 z1) (Vertex x2 y2 z2) (Vertex x3 y3 z3) = Vertex
    ((y2-y1)*(z3-z1) - (y3-y1)*(z2-z1))
    ((z2-z1)*(x3-x1) - (x2-x1)*(z3-z1))
    ((x2-x1)*(y3-y1) - (x3-x1)*(y2-y1))


len :: Vertex -> Double
len (Vertex x y z) = sqrt (sqr x + sqr y + sqr z)
    where sqr v = v * v

unit :: Vertex -> Vertex
unit v@Vertex{..}
    | n == 0 = v
    | otherwise = Vertex (x*n) (y*n) (z*n)
    where n = len v


follow :: [Vertex] -> Vertex
follow vs = Vertex (sum $ map x vs) (sum $ map y vs) (sum $ map z vs)
