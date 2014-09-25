{-# LANGUAGE RecordWildCards, ViewPatterns, ScopedTypeVariables #-}

module Main(main) where

import Development.Shake.Command
import Util
import OBJ
import Surface
import Metadata
import Grouping
import Data.Function
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad
import System.IO
import System.Directory
import System.FilePath
import Control.Arrow


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    copyDirectory "web" "output"

    let Metadata{..} = metadata
    sphere <- readOBJ . lines <$> readFile "data/sphere.obj"
    mdls <- forM datasets $ \dir -> do
        putStr $ "Processing " ++ dir ++ " "
        let name = takeFileName dir

        -- generate the static data
        createDirectoryIfMissing True $ "output/models" </> name
        writeFile ("output/models" </> name </> name <.> "mtl") $ unlines $ concat
            [ let f x = show (fromInteger (toInteger x) / 255.0 :: Double) in
              ["newmtl mtl" ++ name, unwords $ "Kd" : [f r, f g, f b]]
            | (name,(r,g,b)) <- ("SURFACE_FRONT",surfaceFront):("SURFACE_BACK",surfaceBack):
                                map (second fslColor) fossils]
        copyDirectory dir ("output/models" </> name)

        -- read data and validate
        putChar '.'
        surface <- readFileSurface $ dir </> "surface.txt"
        points <- readFilePoints $ dir </> "points.txt"

        {-
        -- automatic surface generation
        let diff = 0.3
        let range = [0,diff .. 10]
        list <- return
            [ (xpos, ypos, 0)
            | ypos <- range, let ymin = ypos - diff, let ymax = ypos + diff
            , let ps = map x $ filter (\Vertex{..} -> y >= ymin && y <= ymax) $ map pointPos $ concatMap snd points
            , let xmin = if null ps then 0 else minimum ps - diff
            , let xmax = if null ps then 0 else maximum ps + diff
            , xpos <- range
            , xpos >= xmin && xpos <= xmax
            ]
        writeFile (dir </> "surface.txt") $ unlines
            [unwords [show x, show y, show z] | (x,y,z) <- list]
        -}

        let assertNoDupes xs = when (nub xs /= xs) $
                error $ "List must not contain duplicates: " ++ show xs
        let assertSubset xs ys = when (xs \\ ys /= []) $
                error $ "Expected subset: " ++ show xs ++ " vs " ++ show ys
        assertNoDupes $ map fst fossils
        map fst points `assertSubset` map fst fossils

        -- generate the obj files
        putChar '.'
        writeFile ("output/models" </> name </> name <.> "obj") $ unlines $ showOBJ $
            [MaterialFile $ name <.> "mtl"] ++
            convertSurface surface ++
            concatMap (convertPoints sphere) points
        putChar '.'
        () <- cmd (Cwd $ "output/models" </> name) Shell $
            "..\\..\\..\\bin\\objcompress " ++ name ++ ".obj " ++ name ++ ".utf8 > " ++ name ++ ".js"
        putStrLn ".\n"

        -- generate the grouping information
        let grp = Grouping
                {grpFile = name <.> "obj"
                ,grpParts = ("SURFACE","Surface") :
                            [(x, fslName $ fromJust $ lookup x fossils) | x <- map fst points]
                ,grpGroups = (("Surface",["SURFACE"]) :) $
                    map (\xs -> (fst $ head xs, map snd xs)) $
                    groupBy ((==) `on` fst) $ sortBy (compare `on` fst)
                    [(fslGroup y,x) | (x, y) <- fossils, x `elem` map fst points]
                }
        writeFile ("output/models" </> name </> "responses.txt") $ unlines $ grouping grp
        () <- cmd (Cwd $ "output/models" </> name) Shell "py ..\\..\\..\\bin\\part_grouping.py < responses.txt"
        () <- cmd (Cwd $ "output/models" </> name) Shell "py ..\\..\\..\\bin\\make_viewer_metadata.py"
        return (name, length $ grpGroups grp)

    writeFile "output/scripts/models.js" $ unlines $
        ["o3v.MODELS = ["] ++
        intercalate [","] [
            ["{"
            ,"  name:'" ++ name ++ ".obj',"
            ,"  scriptName:'" ++ name ++ ".js',"
            ,"  modelPath:'models/" ++ name ++ "/',"
            ,"  metadataFile:'entity_metadata.json',"
            ,"  numLayers:" ++ show count
            ,"}"]
            | (name,count) <- mdls] ++
        ["];"]


readFileSurface :: FilePath -> IO (Surface (Double, Double, Maybe Double))
readFileSurface file = do
    src <- readFile file
    return $ Surface.fromList [(x,y,z) | item <- lines src, let [x,y,z] = map read $ words item]

data Point = Point {pointPos :: Vertex, pointSize :: Vertex, pointAngle :: Double}

readFilePoints :: FilePath -> IO [(String, [Point])]
readFilePoints file = do
    src <- readFile file
    return $ map (\x -> (fst $ head x, map snd x)) $
        groupBy ((==) `on` fst) $ sortBy (compare `on` fst)
        [ (s,Point (Vertex x y z) (Vertex sx sy sz) a)
        | item <- lines src, let s:(map read -> [x,y,z,sx,sy,sz,a]) = words item]


convertPoints :: [OBJ] -> (String,[Point]) -> [OBJ]
convertPoints obj (s,xyz) =
    [Material $ "mtl" ++ s,Group s] ++
    [move pointPos $ rotate pointAngle $ resize pointSize o | Point{..} <- xyz, o <- obj]

resize :: Vertex -> OBJ -> OBJ
resize v (Face vs vns) = Face (map (*v) vs) vns
resize v o = o

rotate :: Double -> OBJ -> OBJ
rotate a (Face vs vns) = Face (map f vs) (map f vns)
    where f Vertex{..} = Vertex (x * cos a - y * sin a) (x * sin a + y * cos a) z
rotate a o = o

move :: Vertex -> OBJ -> OBJ
move v (Face vs vns) = Face (map (+v) vs) vns
move v o = o


convertSurface :: Surface (Double, Double, Maybe Double) -> [OBJ]
convertSurface s =
    [Group "SURFACE",Material "mtlSURFACE_FRONT"] ++
    xs ++ Material "mtlSURFACE_BACK" : map mirror xs
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
