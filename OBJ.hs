{-# LANGUAGE RecordWildCards #-}

module OBJ(Vertex(..), Vector, Face(..), showOBJ) where

import Numeric
import qualified Data.Map as Map


data Vertex = Vertex {x, y, z :: Double} deriving (Eq,Ord)

type Vector = Vertex


data Face = Face {verticies :: [Vertex], vnormal :: Maybe Vector}

data S = S {vs :: Map.Map Vertex Int, vns :: Map.Map Vector Int}

showOBJ :: [Face] -> [String]
showOBJ = showFaces $ S Map.empty Map.empty
    where
        showFaces s [] = []
        showFaces s (f:fs) = showFace s f $ \s -> showFaces s fs

        showFace s Face{..} k =
            showVerticies s verticies $ \s ps ->
            showNormal s vnormal $ \s n ->
            (unwords $ "f" : [p ++ n | p <- ps]) : k s

        showVerticies s [] k = k s []
        showVerticies s (p:ps) k = showVertex s p $ \s i -> showVerticies s ps $ \s is -> k s (i:is)

        showVertex s@S{..} v@Vertex{..} k
            | Just i <- Map.lookup v vs = k s $ show i
            | otherwise = let i = Map.size vs + 1 in
                          (unwords ["v",shw x,shw y,shw z]) :
                          k s{vs = Map.insert v i vs} (show i)

        showNormal s Nothing k = k s ""
        showNormal s@S{..} (Just vn@Vertex{..}) k
            | Just i <- Map.lookup vn vns = k s $ "//" ++ show i
            | otherwise = let i = Map.size vns + 1 in
                          (unwords ["vn",shw x,shw y,shw z]) :
                          k s{vns = Map.insert vn i vns} ("//" ++ show i)


shw x = showFFloat Nothing x ""
