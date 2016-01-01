module FireHazard
(grid,
 instructions,
 lightsLit)
where

import qualified Data.ByteString.Lazy.Char8  as C
import Data.Vector as V (replicate,accum, Vector, foldr, sum)

data Switch = ON | OFF | Toggle deriving (Eq, Show)

type Grid = V.Vector (V.Vector Int)

rv :: V.Vector Int
rv = V.replicate 1000 0

grid :: Grid
grid = V.replicate 1000 rv

lightsLit :: Grid -> Int
lightsLit  g = V.foldr (\e a -> (V.sum e) + a) 0 g

updateCol :: Int -> Switch -> Int
updateCol e v
               | v == Toggle && e == 0 = 1
               | v == Toggle && e == 1 = 0
               | v ==  ON && e == 0 = 1
               | v ==  OFF && e == 1 = 0
               | otherwise = e

updateRow :: V.Vector Int -> [(Int,Switch)] -> V.Vector Int
updateRow v cu = V.accum updateCol v cu

updateGrid :: Grid -> [(Int,[(Int,Switch)])] -> Grid
updateGrid g ru  = V.accum updateRow g ru

switchInst :: [C.ByteString] -> Switch
switchInst bs
            | (head bs) == C.pack "turn" && (head $ tail bs) == C.pack "on" = ON
            | (head bs) == C.pack "toggle" = Toggle
            | otherwise = OFF

bsToGrid :: C.ByteString -> Int
bsToGrid bs = case C.readInt bs of
                Nothing -> 0
                Just (n, _) -> n

startEndInst :: [C.ByteString] -> ([Int], [Int])
startEndInst xs
                  | head xs == C.pack "turn" =
                    (map bsToGrid (C.split ',' (xs !! 2)),
                     map bsToGrid (C.split ',' (xs !! 4)))
                  | otherwise = ( map bsToGrid (C.split ',' (xs !! 1)),
                                  map bsToGrid (C.split ',' (xs !! 3)))

instructions :: C.ByteString -> Grid -> Grid
instructions bs g = updateGrid g r
             where s = C.split ' ' bs
                   si = switchInst s
                   ((cs:rs:_), (ce:re:_)) = startEndInst s
                   c = [(x, si) | x <- [cs..ce]]
                   r = [(y, c) | y <- [rs..re]]
