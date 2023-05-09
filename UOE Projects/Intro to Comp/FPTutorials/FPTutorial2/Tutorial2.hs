  module Tutorial2 where

import Data.Char
import Data.List
import Test.QuickCheck
import Control.Monad (guard)


-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, even x]


-- This is for testing only. Do not try to understand this (yet).
halveEvensReference :: [Int] -> [Int]
halveEvensReference = (>>= \x -> guard (x `mod` 2 == 0) >>= \_ -> return $ x `div` 2)


-- -- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs
    | halveEvens xs == halveEvensReference xs = True
    | otherwise = False


-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, x >= lo && x <= hi]


-- 3. countPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

-- 3.b Even if list comprehension only returns one number, it's still inside of a list
--     thus it's not an Int.

-- 4. multDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits str = product [digitToInt x | x <- str, isDigit x]

countDigits :: String -> Int
countDigits str = length [x | x <- str, isDigit x]

prop_multDigits :: String -> Bool
prop_multDigits xs 
    | multDigits xs <= 9 ^ countDigits xs = True
    | otherwise = False

-- 5. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise (x:xs) = toUpper(x) : [toLower(c) | c <- xs]


-- 6. title

lowercase :: String -> String
lowercase str
  | length str >= 4 = capitalise(str)
  | otherwise = [toLower(x) | x <- str]

-- List-comprehension version
title :: [String] -> [String]
title [] = []
title (x:xs) = capitalise(x) : [lowercase(c) | c <- xs]

-- 7. signs

sign :: Int -> Char
sign i
  | i >= 1 && i <= 9 = '+'
  | i == 0 = '0'
  | i <= -1 && i >= -9 = '-'
  | otherwise = error "the input out of range"

signs :: [Int] -> String
signs xs = [sign(i) | i <- xs, i >= -9 && i <= 9]


-- 8. score

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiouAEIOU"

score :: Char -> Int
score x
  | isAlpha x && isVowel x && isUpper x = 3
  | isAlpha x && (isVowel x || isUpper x) = 2
  | isVowel x && (isAlpha x || isUpper x) = 2
  | isVowel x || isAlpha x || isUpper x = 1
  | otherwise = 0

totalScore :: String -> Int
totalScore xs = product [score x | x <- xs, score x /= 0]

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs
  | totalScore xs >= 1 = True
  | otherwise = False

-- 9. pennypincher

-- List-comprehension version.
pennypincher :: [Int] -> Int
pennypincher prices = sum [round(fromIntegral price * 0.9) | price <- prices, (fromIntegral price * 0.9) <= 19900]

-- -- And the test itself
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs <= sum [x | x <- xs, x <= 19900 && x >= 0]

-- Optional Material

-- 10. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [word | word <- words, idWord word == True, isLetter letter == True]
    where 
  idWord word
        | pos > len-1 || pos < 0 || len <= 0 || len > length word = False
        | letter `elem` word && letter == word!!pos && len == length word = True
        | otherwise = False


-- 11. search

-- List-comprehension version

search :: String -> Char -> [Int]
search str goal = [i | i <- [0..(length(str)-1)], goal==str!!i]

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal
  | length(search str goal) >= 0 = True
  | otherwise = False


-- 12. contains

contains :: String -> String -> Bool
contains str substr
  | null [x | x <- [0..(length(str)-1)], isPrefixOf substr (drop x str)] == False = True
  | str==substr && str == "" = True
  | otherwise = False
  

-- Depending on the property you want to test, you might want to change the type signature
prop_contains :: String -> String -> Bool
prop_contains str1 str2
  | length(str1) < length(str2) && contains str1 str2 == True = False
  | otherwise = True