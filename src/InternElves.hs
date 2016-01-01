module InternElves
(nice)
where

import qualified Data.ByteString.Lazy.Char8 as C

vowels = ['a','e','i','o','u']

bad = [('a','b'),('c','d'),('p','q'),('x','y')]

isBadString :: (Char, Char) -> [Bool] -> [Bool]
isBadString e xs
                 | elem e bad = True : xs
                 | otherwise = xs

isTwice :: (Char, Char) ->[Bool] -> [Bool]
isTwice e xs
             | fst e == snd e = True : xs
             | otherwise = xs

countVowels :: Char -> Int -> Int
countVowels e a
               | elem e vowels = a + 1
               | otherwise = a

atLeastThreeVowels :: C.ByteString -> Bool
atLeastThreeVowels xs = (C.foldr countVowels 0 xs) >= 3

anyBadString :: [(Char, Char)] -> Bool
anyBadString xs = any (\a -> a == True) (foldr isBadString [] xs)

anyTwice :: [(Char, Char)] -> Bool
anyTwice xs = any (\a -> a == True) (foldr isTwice [] xs)

nice :: C.ByteString -> Bool
nice bs = (anyTwice z) && (not .anyBadString) z && (atLeastThreeVowels bs)
    where t = C.tail bs
          z = C.zip bs t
