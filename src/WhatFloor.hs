module WhatFloor
(whatFloor)
where

import qualified Data.ByteString.Lazy.Char8 as C

whatFloor :: C.ByteString -> Int
whatFloor = C.foldr upDown 0

upDown :: Char -> Int -> Int
upDown c a
         | c == '(' = a + 1
         | c == ')' = a - 1
         | otherwise = a
