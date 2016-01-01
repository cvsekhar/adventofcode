module Lib where

import qualified Data.ByteString.Lazy.Char8 as C
import WhatFloor
import WrappingPaper
import OnePresent
import InternElves
import FireHazard

floor' :: [C.ByteString] -> Int
floor' = foldr (\e a -> a + (whatFloor e)) 0

orderPaper :: [C.ByteString] -> Int
orderPaper = foldr (\e a -> a + (slack $ getDimensions e)) 0

orderRibbon :: [C.ByteString] -> Int
orderRibbon = foldr (\e a -> a + (ribbon $ getDimensions e)) 0

presents :: [C.ByteString] -> Int
presents = foldr (\e a -> a + (numberOfHouses e)) 0

niceStrings :: [C.ByteString] -> Int
niceStrings = foldr (\e a -> if nice e then a + 1 else a) 0

lits :: [C.ByteString] -> Int
lits bss = lightsLit $ foldl (\a e -> instructions e a) grid bss
