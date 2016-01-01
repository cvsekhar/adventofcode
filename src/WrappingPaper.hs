module WrappingPaper
( slack,
  ribbon,
  getDimensions )
where

import qualified Data.ByteString.Lazy.Char8 as C

slack :: (Maybe (Int, C.ByteString) ,
          Maybe (Int, C.ByteString) ,
          Maybe (Int, C.ByteString) ) -> Int
slack (Just(l,_), Just(w,_), Just(h,_)) = let
                      part = map (\(x,y) -> x * y) [(l,w),(w,h),(h,l)]
                      extra = minimum part
                   in
                      2 * sum part + extra
slack (_,_,_) = 0

ribbon :: (Maybe (Int, C.ByteString) ,
          Maybe (Int, C.ByteString) ,
          Maybe (Int, C.ByteString) ) -> Int
ribbon (Just(l,_), Just(w,_), Just(h,_)) = let
                      part = map (\(x,y) -> x + y) [(l,w),(w,h),(h,l)]
                      extra = l * w * h
                   in
                      2 * minimum part + extra
ribbon (_,_,_) = 0

getDimensions :: C.ByteString -> (Maybe (Int, C.ByteString) ,
                                  Maybe (Int, C.ByteString) ,
                                  Maybe (Int, C.ByteString) )
getDimensions b = let
                    l:w:h:_ = C.split 'x' b
                  in
                    (C.readInt l, C.readInt w, C.readInt h)
