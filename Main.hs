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
import System.IO


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putChar '.'
    copyDirectory "web" "output"
    copyDirectory "data/e" "output/models/e"
    copyFile "data/materials.mtl" "output/models/e/materials.mtl"
    surface <- readFileSurface "data/e/surface.txt"
    points <- readFilePoints "data/e/points.txt"
    groups <- (("surf",["surface"]):) <$> readFileGroups "data/e/groups.txt"
    sphere <- readOBJ . lines <$> readFile "data/sphere.obj"
    putChar '.'
    writeFile "output/models/e/e.obj" $ unlines $ showOBJ $
        [MaterialFile "materials.mtl"] ++
        convertSurface surface ++
        concatMap (convertPoints sphere) points
    putChar '.'
    () <- cmd (Cwd "output/models/e") Shell "..\\..\\..\\bin\\objcompress e.obj e.utf8 > e.js"
    putStrLn ".\n"
    let n = length groups
    writeFile "output/models/e/responses.txt" $ unlines $
        ["e.obj","0",show n] ++
        concatMap (replicate 2 . fst) groups ++
        ["e"] ++
        map show [1..n] ++
        concat [ map show $ i : length vs : map (succ . fromJust . flip elemIndex lst) vs
               | let lst = ("surface":map fst points)
               , (i,(_,vs)) <- zip [1..n] groups]
    () <- cmd (Cwd "output/models/e") Shell "py ..\\..\\..\\bin\\part_grouping.py < responses.txt"
    () <- cmd (Cwd "output/models/e") Shell "py ..\\..\\..\\bin\\make_viewer_metadata.py"
    writeFile "output/scripts/models.js" $ unlines
        ["o3v.MODELS = [{"
        ,"  name:'e.obj',"
        ,"  scriptName:'e.js',"
        ,"  modelPath:'models/e/',"
        ,"  metadataFile:'entity_metadata.json',"
        ,"  numLayers:" ++ show n
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

readFileGroups :: FilePath -> IO [(String, [String])]
readFileGroups file = do
    src <- readFile file
    return [(x,xs) | x:xs <- map words $ lines src]


convertPoints :: [OBJ] -> (String,[Vertex]) -> [OBJ]
convertPoints obj (s,xyz) =
    [Material $ "mtl" ++ s,Group s] ++
    [Face (map ((+v) . (*0.03)) vs) vns | v <- xyz, Face vs vns <- obj]


convertSurface :: Surface (Double, Double, Maybe Double) -> [OBJ]
convertSurface s =
    [Group "surface",Material "mtlFRONT"] ++
    xs ++ Material "mtlBACK" : map mirror xs
    where xs = collect (faces (allNormals s) s)

mirror :: OBJ -> OBJ
mirror (Face vs ns) = Face (reverse vs) (map negate ns)
mirror x = x

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
