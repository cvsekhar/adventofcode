module OnePresent
(present,
 markHouse,
 numberOfHouses)
where

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map.Lazy as Map

type House = (Int, Int)

present :: C.ByteString -> Map.Map House Int
present = fst . C.foldl' markHouse (Map.fromList [((0,0),1)], (0,0))

markHouse :: (Map.Map House Int, House) -> Char -> (Map.Map House Int, House)
markHouse (m, k) c
                  | c == '>' = update m (f+1, s)
                  | c == '<' = update m (f-1, s)
                  | c == '^' = update m (f, s+1)
                  | c == 'v' = update m (f, s-1)
                  | otherwise = (m, k)
          where f = fst k
                s = snd k

update :: Map.Map House Int -> House -> (Map.Map House Int, House)
update m (f, s) = (Map.insert (f, s) 1 m, (f, s))

numberOfHouses :: C.ByteString -> Int
numberOfHouses bs = Map.foldr (+) 0 (present bs)
