{-# LANGUAGE RecordWildCards #-}

module Main(main) where

import Development.Shake.Command
import System.Directory
import Util
import OBJ


main :: IO ()
main = do
    copyDirectory "web" "output"
    copyDirectory "data/set1" "output/models/set1"
    src <- readFile "data/set1/surface.txt"
    copyFile "data/materials.mtl" "output/models/set1/materials.mtl"
    writeFile "output/models/set1/set1.obj" $ convert src
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


convert lst = unlines $
    ["mtllib materials.mtl","usemtl mtlsurface","g surface"] ++
    showOBJ (concat
        [[Face [snd o , snd x, snd y] (Just $ normal (snd o)  (snd x) (snd y))
         ,Face [snd xy, snd y, snd x] (Just $ normal (snd xy) (snd y) (snd x))]
        | (i, (o, x, y, xy)) <- tri])
    where
        rpts = reverse pts
        pts = [Vertex x y z | item <- lines lst, let [x,y,z] = map read $ words item, not $ isNaN z]

        tri = zip [0..] [((i, o), x, y, xy)
            | (i,o) <- zip [0..] pts
            , Just x  <- [findIndexValue (\x -> getX x >  getX o && getY x == getY o) pts]
            , Just y  <- [findIndexValue (\y -> getX y == getX o && getY y >  getY o) pts]
            , Just xy <- [findIndexValue (\xy -> getX (snd x) == getX xy && getY (snd y) == getY xy) pts]
            ]

getX Vertex{..} = x
getY Vertex{..} = y
getZ Vertex{..} = z


