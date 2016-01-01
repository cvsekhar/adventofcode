module WrappingPaper
( slack,
  getDimensions )
where

import qualified Data.ByteString.Lazy.Char8 as C

slack :: (Maybe (Int, C.ByteString) ,
          Maybe (Int, C.ByteString) ,
          Maybe (Int, C.ByteString) ) -> Int
slack (Just(l,_), Just(w,_), Just(h,_)) = let
                      part1 = l * w
                      part2 = w * h
                      part3 = h * l
                      extra = minimum [part1, part2, part3]
                   in
                      2 * (part1+part2+part3) + extra
slack (_,_,_) = 0

getDimensions :: C.ByteString -> (Maybe (Int, C.ByteString) ,
                                  Maybe (Int, C.ByteString) ,
                                  Maybe (Int, C.ByteString) )
getDimensions b = let
                    l:w:h:_ = C.split 'x' b
                  in
                    (C.readInt l, C.readInt w, C.readInt h)
