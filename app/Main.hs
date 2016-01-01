module Main where

import Lib
import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = do
   content <- C.readFile "files/presents.txt"
   let linesOfFile = C.lines content
   putStrLn (show $ presents linesOfFile)
   return ()
