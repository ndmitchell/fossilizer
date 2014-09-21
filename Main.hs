
module Main(main) where

import Numeric
import Development.Shake.Command
import System.Directory
import Util


main :: IO ()
main = do
    copyDirectory "web" "output"
    copyDirectory "data/set1" "output/models/set1"
    src <- readFile "data/set1/surface.txt"
    copyFile "data/materials.mtl" "output/models/set1/materials.mtl"
    writeFile "output/models/set1/set1.obj" $ convert src
    () <- cmd (Cwd "output/models/set1") Shell "..\\..\\..\\bin\\objcompress set1.obj set1.utf8 > set1.js"
    writeFile "output/models/set1/responses.txt" $ unlines
        ["set1.obj"
        ,"0"
        ,"1"
        ,"surf"
        ,"surf"
        ,"set1"
        ,"1"
        ,"1"
        ,"1"
        ,"1"
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
    [unwords ["v",shw x,shw y,shw z] | (x,y,z) <- pts] ++
    concat [[let (x_,y_,z_) = normal (snd o) (snd x) (snd y) in unwords ["vn",shw x_,shw y_,shw z_]
            ,let op n = show (n+1) ++ "//" ++ show (i*2+1) in unwords ["f",op $ fst o,op $ fst x,op $ fst y]
            ,let (x_,y_,z_) = normal (snd xy) (snd y) (snd x) in unwords ["vn",shw x_,shw y_,shw z_]
            ,let op n = show (n+1) ++ "//" ++ show (i*2+2) in unwords ["f",op $ fst xy,op $ fst y,op $ fst x]]
        | (i, (o, x, y, xy)) <- tri]
    where
        rpts = reverse pts
        pts = [(x,y,z) | item <- lines lst, let [x,y,z] = map read $ words item, not $ isNaN z]

        tri = zip [0..] [((i, o), x, y, xy)
            | (i,o) <- zip [0..] pts
            , Just x  <- [findIndexValue (\x -> getX x >  getX o && getY x == getY o) pts]
            , Just y  <- [findIndexValue (\y -> getX y == getX o && getY y >  getY o) pts]
            , Just xy <- [findIndexValue (\xy -> getX (snd x) == getX xy && getY (snd y) == getY xy) pts]
            ]

shw x = showFFloat Nothing x ""

getX (x,_,_) = x
getY (_,y,_) = y
getZ (_,_,z) = z


normal :: (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double)
normal (x1,y1,z1) (x2,y2,z2) (x3,y3,z3) =
    ( (y2-y1)*(z3-z1) - (y3-y1)*(z2-z1)
    , (z2-z1)*(x3-x1) - (x2-x1)*(z3-z1)
    , (x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)
    )
