{-# LANGUAGE RecordWildCards #-}

module Grouping(Grouping(..), grouping) where

import Data.Maybe
import Data.List
import System.FilePath


data Grouping = Grouping
    {grpFile :: FilePath
    ,grpParts :: [(String, String)] -- (part name, part nice name), must be in file order
    ,grpGroups :: [(String, [String])] -- (group nice name, parts included)
    }

-- Create a sample stream which is suitable for feeding to part_grouping.py
grouping :: Grouping -> [String]
grouping Grouping{..} =
    -- name of the file
    [grpFile] ++
    -- do you want to give nice names for the parts
    ["1"] ++
    -- nice names for each part
    map snd grpParts ++
    -- how many groups do you have
    [show $ length grpGroups] ++
    -- groups and their nice names
    concat [["grp" ++ show i, name] | (i,(name,_)) <- zip [1..] grpGroups] ++
    -- name of the whole thing
    [takeBaseName grpFile] ++
    -- indexes of each group
    map show [1..length grpGroups] ++
    -- index of group, number of parts, index of parts
    concat [ show i : show (length parts) : map (show . indPart) parts
           | (i,(_,parts)) <- zip [1..] grpGroups
           , let indPart p = fromJust $ findIndex ((==) p . fst) grpParts
           ]
