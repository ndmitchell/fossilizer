{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Main(main) where

import Development.Shake.Command
import System.Directory
import Util
import OBJ
import Surface
import Data.Maybe


main :: IO ()
main = do
    copyDirectory "web" "output"
    copyDirectory "data/set1" "output/models/set1"
    copyFile "data/materials.mtl" "output/models/set1/materials.mtl"
    src <- readFileSurface "data/set1/surface.txt"
    writeFile "output/models/set1/set1.obj" $ unlines $
        ["mtllib materials.mtl","usemtl mtlsurface","g surface"] ++
        showOBJ (convert src)
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


convert :: Surface (Double, Double, Maybe Double) -> [Face]
convert = collect . faces


collect :: Surface (Maybe a, Maybe a) -> [a]
collect s = concat [maybeToList a ++ maybeToList b | (a,b) <- toList s]


faces :: Surface (Double, Double, Maybe Double) -> Surface (Maybe Face, Maybe Face)
faces s = flip ffmap s $ \x y _ ->
    (triangle (x,y) (pred x,y) (x,pred y)
    ,triangle (x,y) (succ x,y) (x,succ y))
    where
        triangle (f -> Just v1) (f -> Just v2) (f -> Just v3) = Just $ Face [v1, v2, v3] $ replicate 3 $ normal v1 v2 v3
        triangle _ _ _ = Nothing

        f xy = case s !? xy of
            Just (x,y,Just z) -> Just $ Vertex x y z
            _ -> Nothing
