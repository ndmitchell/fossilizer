{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Main(main) where

import Development.Shake.Command
import System.Directory
import Util
import OBJ
import Surface
import Data.Function
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad


main :: IO ()
main = do
    copyDirectory "web" "output"
    copyDirectory "data/set1" "output/models/set1"
    copyFile "data/materials.mtl" "output/models/set1/materials.mtl"
    surface <- readFileSurface "data/set1/surface.txt"
    points <- readFilePoints "data/set1/points.txt"
    sphere <- readOBJ . lines <$> readFile "data/sphere.obj"
    writeFile "output/models/set1/set1.obj" $ unlines $ showOBJ $
        [MaterialFile "materials.mtl"] ++
        convertSurface surface ++
        concatMap (convertPoints sphere) points
    () <- cmd (Cwd "output/models/set1") Shell "..\\..\\..\\bin\\objcompress set1.obj set1.utf8 > set1.js"
    writeFile "output/models/set1/responses.txt" $ unlines
        ["set1.obj","0","1"
        ,"surf","surf","set1"
        ,"1","1","1","1"
        ]
    () <- cmd (Cwd "output/models/set1") Shell "py ..\\..\\..\\bin\\part_grouping.py < responses.txt"
    () <- cmd (Cwd "output/models/set1") Shell "py ..\\..\\..\\bin\\make_viewer_metadata.py"
    writeFile "output/scripts/models.js" $ unlines
        ["o3v.MODELS = [{"
        ,"  name:'set1.obj',"
        ,"  scriptName:'set1.js',"
        ,"  modelPath:'models/set1/',"
        ,"  metadataFile:'entity_metadata.json',"
        ,"  numLayers:1"
        ,"}];"
        ]
    return ()


readFileSurface :: FilePath -> IO (Surface (Double, Double, Maybe Double))
readFileSurface file = do
    src <- readFile file
    return $ Surface.fromList [(x,y,z) | item <- lines src, let [x,y,z] = map read $ words item]

readFilePoints :: FilePath -> IO [(String, [Vertex])]
readFilePoints file = do
    src <- readFile file
    return $ map (\x -> (fst $ head x, map snd x)) $
        groupBy ((==) `on` fst) $ sortBy (compare `on` fst)
        [(s,Vertex (read x) (read y) (read z)) | item <- lines src, let [x,y,z,s] = words item]


convertPoints :: [OBJ] -> (String,[Vertex]) -> [OBJ]
convertPoints obj (s,xyz) =
    [Material $ "mtl" ++ s] ++
    [Face (map ((+v) . (*0.05)) vs) vns | v <- xyz, Face vs vns <- obj]


convertSurface :: Surface (Double, Double, Maybe Double) -> [OBJ]
convertSurface s =
    [Material "mtlsurface",Group "surface"] ++
    collect (faces (allNormals s) s)


collect :: Surface (Maybe a, Maybe a) -> [a]
collect s = concat [maybeToList a ++ maybeToList b | (a,b) <- toList s]


faces :: Surface Vertex -> Surface (Double, Double, Maybe Double) -> Surface (Maybe OBJ, Maybe OBJ)
faces norms s = flip ffmap s $ \x y _ ->
    (triangle (x,y) (pred x,y) (x,pred y)
    ,triangle (x,y) (succ x,y) (x,succ y))
    where
        triangle p1@(f -> Just v1) p2@(f -> Just v2) p3@(f -> Just v3) =
            Just $ Face [v1, v2, v3] $ map (norms !) [p1,p2,p3]
        triangle _ _ _ = Nothing

        f xy = case s !? xy of
            Just (x,y,Just z) -> Just $ Vertex x y z
            _ -> Nothing


allNormals :: Surface (Double, Double, Maybe Double) -> Surface Vertex
allNormals s = flip ffmap s $ \x y _ -> unit $ sum $ map unit $ mapMaybe g
    [(x,y,pred x,y,x,pred y)
    ,(x,y,x,pred y,succ x,y)
    ,(x,y,succ x,y,x,succ y)
    ,(x,y,x,succ y,pred x,y)
    ]
    where
        g (x1,y1,x2,y2,x3,y3) = liftM3 normal (f (x1,y1)) (f (x2,y2)) (f (x3,y3))

        f xy = case s !? xy of
            Just (x,y,Just z) -> Just Vertex{..}
            _ -> Nothing
