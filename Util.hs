
module Util where

import Data.List
import System.Directory
import Control.Monad
import System.FilePath


findIndexValue :: (a -> Bool) -> [a] -> Maybe (Int, a)
findIndexValue f xs = find (f . snd) $ zip [0..] xs


copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory from to = do
    xs <- getDirectoryContentsRecursive from
    forM_ xs $ \x -> do
        let dest = to ++ drop (length from) x
        createDirectoryIfMissing True $ takeDirectory dest
        copyFile x dest


getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir = do
    xs <- getDirectoryContents dir
    (dirs,files) <- partitionM doesDirectoryExist [dir </> x | x <- xs, not $ isBadDir x]
    rest <- concatMapM getDirectoryContentsRecursive $ sort dirs
    return $ sort files ++ rest
    where
        isBadDir x = "." `isPrefixOf` x || "_" `isPrefixOf` x

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f [] = return ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    return ([x | res]++as, [x | not res]++bs)

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

