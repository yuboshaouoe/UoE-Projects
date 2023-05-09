module Tutorial3 where

import Data.Char
import Data.List
import Data.Tuple
import Test.QuickCheck


-- 1.

halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs)
      | even x = x `div` 2 : halveEvensRec xs
      | otherwise = halveEvensRec xs

halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvensRec xs == halveEvens xs


-- 2.

inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec a b [] = []
inRangeRec a b (x:xs)
      | x < a || x > b = inRangeRec a b xs
      | otherwise = x : inRangeRec a b xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRange lo hi xs == inRangeRec lo hi xs


-- 3.

countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs)
      | x > 0 = 1 + countPositivesRec xs
      | otherwise = countPositivesRec xs

countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

prop_countPositives :: [Int] -> Bool
prop_countPositives l = countPositivesRec l == countPositives l


-- 4.

multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs)
      | isDigit x == True = digitToInt x * multDigitsRec xs
      | otherwise = multDigitsRec xs

multDigits :: String -> Int
multDigits str = product [digitToInt ch | ch <- str, isDigit ch]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs == multDigitsRec xs


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- Ceasar Cipher Exercises
-- =======================


-- 5.

lookUp :: Char -> [(Char, Char)] -> Char
lookUp ch xs
      | [snd x | x <- xs, ch == fst x] == [] = ch
      | otherwise = head [snd x | x <- xs, ch == fst x]

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec ch [] = ch
lookUpRec ch (x:xs)
      | ch == fst x = snd x
      | otherwise = lookUpRec ch xs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c k = lookUp c k == lookUpRec c k 


-- 6.

encipher :: Int -> Char -> Char
encipher k ch = lookUp ch (makeKey k)


-- 7.

normalize :: String -> String
normalize chars = [toUpper(char) | char <- chars, isAlpha char || isDigit char == True]


encipherStr :: Int -> String -> String
encipherStr k str = [encipher k char | char <- normalize str]


-- 8.

reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey chars = map swap chars

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec (x:xs) = swap x : reverseKeyRec xs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey chars = reverseKey chars == reverseKeyRec chars


-- 9.

decipher :: Int -> Char -> Char
decipher k ch = lookUp ch (reverseKey (makeKey k))

decipherStr :: Int -> String -> String
decipherStr k chars = [decipher k char | char <- normalize chars]

-- Optional Material
-- =================


-- 10.

contains :: String -> String -> Bool
contains strA strB = not (null [k | k <- [0..length strA], isPrefixOf strB (drop k strA)])


-- 11.

candidates :: String -> [(Int, String)]
candidates chars= [(k, decipherStr k chars) | k <- [0..25], 
                  contains (decipherStr k chars) "THE" || contains (decipherStr k chars) "AND"]


-- 12.

add_X :: String -> String
add_X str
      | 5-(length(str) `mod` 5) < 5 = str ++ replicate (5-(length(str) `mod` 5)) 'X'
      | otherwise = str

splitEachFive :: String -> [String]
splitEachFive "" = ["XXXXX"]
splitEachFive str = [take 5 (drop (5*k) (add_X str)) | k <- [0..(length(add_X str) `div` 5)-1]]

prop_transpose :: String -> Bool
prop_transpose str = transpose(transpose(splitEachFive str)) == splitEachFive str


-- 13.
encrypt :: Int -> String -> String
encrypt k str = concat(transpose(splitEachFive(encipherStr k str)))


-- 14.

--There is no way to output THE EXACT input of encrypt function using any decrypt function.
--The best that can be done is output a range of possible outputs which contains the initial string.
--For example, input string "XXXXXX" into encrypt function, which is going to output ["XXXXX", "XXXXX"] after the splitEachFive step.
--There is no way to confirm the original string as it can be any one of "XXXXXX", "XXXXXXX", "XXXXXXXX", "XXXXXXXXX", "XXXXXXXXXX" when decrypting.
--Secondly, after normalization, the space and notations in the initial string are normalized,
--there is no way of knowing if there was a question mark in the initial string when decrypting.
--Lastly, the initial k value is not known.
--Thus, the best decrypt function that outputs A SINGLE STRING is to return a normalized decrypted string with all Xs removed at a random k value.
--However, for any long text, the candidate function can be used and pick out the candidate with the most "THE" and/or "AND" and return that.


anticoncat :: String -> [String]
anticoncat str = [take (length(splitEachFive str)) (drop ((length(splitEachFive str))*k) (add_X str)) 
                  | k <- [0..(length(add_X str) `div` (length(splitEachFive str)))-1]]

--antiSplit :: [String] -> String
--antiSplit xss = [x | x <-(concat xss), x /= 'X']

decrypt :: Int -> String -> String
decrypt k str = decipherStr k (concat(transpose(anticoncat str)))

decryptAllPossibility :: String -> [String]
decryptAllPossibility str = [decrypt k str | k <- [0..25]]

--This works for all strings, it tries all 26 possible k values and you can check the possible initial string for yourself.


-- intCandidates :: String -> [Int]
-- intCandidates str = [k | k <- [0..25], contains (decipherStr k (antiSplit(transpose(anticoncat str)))) "THE" || contains (decipherStr k (antiSplit(transpose(anticoncat str)))) "AND"]

-- decryptMostPossibleLongString :: String -> [String]
-- decryptMostPossibleLongString str = [decrypt k str | k <- intCandidates str]

--This works better for long strings, as it only looks for the one containing "THE" or "AND" in the string's normalized form.
--I can keep working to get it to only return TOP 3 candidates with the most number of "THE"/"AND", but it's somewhat unecessary.