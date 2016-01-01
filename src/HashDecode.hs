module HashDecode
(md5,
 md5check)
where

import qualified Data.ByteString.Char8  as C (pack,take)
import Crypto.Hash

hardLimit = 99999999

n = 6

halfKey = "yzbqklnj"

zero = C.pack $ replicate n '0'

md5 :: String -> Digest MD5
md5 s = hash $ C.pack s

md5check :: String -> Int -> Maybe Int
md5check s i
           | i > hardLimit = Nothing
           | t5 == zero = Just i
           | otherwise = md5check s (i+1)
      where  m5 = md5 (s ++ show i)
             dig = digestToHexByteString m5
             t5 = C.take n dig
