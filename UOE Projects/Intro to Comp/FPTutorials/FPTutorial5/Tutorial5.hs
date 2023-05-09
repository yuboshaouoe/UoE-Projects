{-# LANGUAGE RankNTypes #-}
module Tutorial5 where

import Data.Char
import Data.List
import Test.QuickCheck


-- 1. Map
-- a.
doubles :: [Int] -> [Int]
doubles xs = map (2*) xs

-- b.        
penceToPounds :: [Int] -> [Float]
penceToPounds xs = map (/100) (map fromIntegral xs)

-- c.
uppers :: String -> String
uppers xs = map toUpper xs

-- d.
uppersComp :: String -> String
uppersComp xs = [toUpper x | x <- xs]

prop_uppers :: String -> Bool
prop_uppers s = uppers s == uppersComp s


-- 2. Filter
-- a.
alphas :: String -> String
alphas xs = filter isAlpha xs

-- b.
above :: Int -> [Int] -> [Int]
above n xs = filter (>n) xs

-- c.
unequal :: (Int,Int) -> Bool
unequal (x,y) = if x/=y then True else False

unequals :: [(Int,Int)] -> [(Int,Int)]
unequals [] = []
unequals xs = filter unequal xs

-- d.
rmChar :: Char -> String -> String
rmChar char xs = filter (/=char) xs 

-- e.
rmCharComp :: Char -> String -> String
rmCharComp char xs = [x | x <- xs, x /= char]

prop_rmChar :: Char -> String -> Bool
prop_rmChar c s = rmChar c s == rmCharComp c s


-- 3. Comprehensions vs. map & filter
-- a.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = map (2*) (filter (>3) xs)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- b.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

lengthEven :: String -> Bool
lengthEven str = if even(length str) then True else False

reverseEven' :: [String] -> [String]
reverseEven' strs = map reverse (filter lengthEven strs)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs

-- 4. Foldr
-- a.
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs)= x && andRec xs

andFold :: [Bool] -> Bool
andFold bs = foldr (&&) True bs

prop_andFold :: [Bool] -> Bool
prop_andFold list = andFold list == andRec list  

-- b.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold [] = []
concatFold (x:xs) = foldr (++) ([]) (x:xs)

prop_concat :: (Eq a) => [[a]] -> Bool
prop_concat [xs] = concatFold [xs] == concatRec [xs]

-- c.
rmCharsRec :: String -> String -> String
rmCharsRec [] str = str
-- rmCharsRec (x:xs) str = rmCharsRec xs (rmChar x str)
rmCharsRec (x:xs) str = rmChar x (rmCharsRec xs str)

rmCharsFold :: String -> String -> String
rmCharsFold [] ys = ys
rmCharsFold xs ys = foldr rmChar ys xs

prop_rmChars :: String -> String -> Bool
prop_rmChars xs ys = rmCharsRec xs ys == rmCharsFold xs ys


-- Matrix multiplication

type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform [] = True
uniform (x:xs) = all (==x) (x:xs)

-- b.

valid :: Matrix -> Bool
valid [] = False
valid xs
    | length xs >= 1 && uniform ((map length xs)) && not(null(filter (/=[]) xs))= True
    | otherwise = False

-- 6.
matrixWidth :: Matrix -> Int
matrixWidth (x:xs)
    | valid (x:xs) = length(x)
    | otherwise = error "input is not a matrix"

matrixHeight :: Matrix -> Int
matrixHeight (x:xs)
    | valid (x:xs) = length ((x:xs))
    | otherwise = error "input is not a matrix"

plusM :: Matrix -> Matrix -> Matrix
plusM [] [] = []
plusM (x:xs) (y:ys)
    | matrixHeight (x:xs) == matrixHeight (y:ys) && 
      matrixWidth (x:xs) == matrixWidth (y:ys) = 
      [zipWith (+) x y] ++ plusM xs ys
    | otherwise = error "Matrices are not suitable"

-- 7.

timesM :: Matrix -> Matrix -> Matrix
timesM a b
    | matrixHeight a == matrixWidth b || 
      matrixHeight b == matrixWidth a = 
      [[sum(zipWith (*) arow bcolumn) | bcolumn <- (transpose b)] | arow <- a]
    | otherwise = error "Matrices are not suitable"

-- 8.
-- b.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [x `f` y | (x,y) <- zip xs ys]

-- c.
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = map (uncurry f) (zip xs ys)

-- ** Optional material

-- 9.

-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = undefined

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f = undefined

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]     
removes = undefined

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined
        
determinant :: Matrix -> Rational
determinant = undefined

cofactors :: Matrix -> Matrix
cofactors m = undefined        
                
scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

prop_inverse2 :: Rational -> Rational -> Rational 
                -> Rational -> Property
prop_inverse2 a b c d = undefined

type Triple a = (a,a,a)
        
prop_inverse3 :: Triple Rational -> 
                 Triple Rational -> 
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = undefined
