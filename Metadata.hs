{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Metadata(
    Metadata(..), Fossil(..), Color,
    metadata
    ) where

type Color = (Int,Int,Int)

data Metadata = Metadata
    {surfaceFront :: Color
    ,surfaceBack :: Color
    ,datasets :: [FilePath]
    ,fossils :: [(String, Fossil)]
    }

data Fossil = Fossil
    {fslName :: String
    ,fslColor :: Color
    ,fslGroup :: String
    }

metadata :: Metadata
metadata = Metadata brown darkGray ["data/e","data/h"]
    [fossil "BRA" "Bradgatia" pink ""
    ,fossil "CHAR" "Charniid" purple ""
    ,fossil "LOB" "Lobate Discs" lightGreen ""
    ,fossil "IVE" "Ivesheadiamorphs" darkGreen ""
    ,fossil "FRA" "Fractofusus" blue ""
    ,fossil "FRA_SMALL" "Fractofusus (small)" darkBlue ""
    ,fossil "FRA_MEDIUM" "Fractofusus (medium)" blue ""
    ,fossil "FRA_LARGE" "Fractofusus (large)" lightBlue ""
    ,fossil "CHA" "Charniodiscus" red "Fronds"
    ,fossil "FEA" "Feather dusters" yellow "Fronds"
    ,fossil "HIEM" "Hiemalora" yellow "Fronds"
    ,fossil "DISC" "Holdfast disc" orange "Fronds"
    ,fossil "THE" "Thectardis" black ""
    ,fossil "OTHER" "Others" gray ""
    ,fossil "PEC" "Pectinifrons" yellow ""
    ]
    where fossil a b c d = (a, Fossil b c $ if d == "" then b else d)


---------------------------------------------------------------------
-- COLORS

colorD a b c = let f = round . (* 255) in (f a, f b, f c)
colorI = (,,)

brown = colorI 156 120 42
darkGray = colorD 0.20 0.20 0.20
pink = colorD 0.95 0.34 0.80
purple = colorD 0.59 0.24 0.72
lightBlue = colorI 52 187 224
blue = colorI 0 68 255
darkBlue = colorI 6 2 122
lightGreen = colorI 144 232 164
darkGreen = colorI 12 122 38
red = colorD 0.91 0.13 0.13
yellow = colorD 1.00 0.93 0.00
orange = colorD 1.00 0.30 0.00
black = colorD 0.00 0.00 0.00
gray = colorD 0.50 0.50 0.50
