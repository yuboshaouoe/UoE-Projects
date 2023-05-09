module FinalExam where

import Test.QuickCheck
import Control.Monad
import Data.Char

{-
For each question, a quickCheck property is added below.
Each of them has passed quickCheck.
Also, every example given has also been passed.
-}

-- Question 1

-- 1a

-- The answers below assumes the input list has been sorted
-- by dictionary order.

{- 
The checkLength function below allows me to do this:
    *FinalExam> f ["vvvvvv","vvvvv","vvvv","wwwww","www"]
    "vvvv"
Till I learned that length is also sorted in dictionary order.
What a bummer.
By the way, why isn't null a library function?
had to use == [] instead.
-}

{-
checkLength :: [String] -> String
checkLength strs | head check == head strs = head strs
                 | otherwise = strs!!(length heads-1)
  where check = [take (length(strs!!(n+1))) (strs!!n) | n <- [0..length strs-2]]
        heads = [head a | a <- check, head a == head(head check)]
-}

satisfies :: [String] -> [String]
satisfies strs = [str | str <- strs, and[isLower c | c <- str], length str < 6]

f :: [String] -> String
f strs | concat strs == "" = ""
       | satisfies strs == [] = "zzzzz"
       | otherwise = head $ satisfies strs
        
-- 1b

lower :: String -> Bool
lower [] = True
lower (x:xs) | isLower x = lower xs
             | otherwise = False

g' :: [String] -> String
g' [] = "zzzzz"
g' (x:xs) | length x < 6 && lower x = x
          | otherwise = g' xs

g :: [String] -> String
g [] = ""
g (x:xs) = g' (x:xs)

-- 1c
lower' :: String -> Bool
lower' str = filter isLower str == str

h :: [String] -> String
h [] = ""
h xs =  if filter lower' (filter (\n -> length n < 6) xs) == []
        then "zzzzz"
        else head'(filter lower' (filter (\n -> length n < 6) xs))
      where head'(x:xs) = x

prop_fgh :: [String] -> Bool
prop_fgh strs = f strs == g strs && g strs == h strs && f strs == h strs

-- Question 2

-- 2a

i :: [a] -> [a] -> [a]
i xs ys = tail xs ++ [head ys]

-- 2b

j :: [[a]] -> [[a]]
j xs = [i (xs!!n) (xs!!(n+1)) | n <- [0..length xs-2]] 
       ++ [i (last xs) (head xs)]

-- 2c
k' :: [[a]] -> [a]
k' [] = []
k' [x] = x
k' (x:xs) = k' xs

k'' :: [[a]] -> [[a]]
k'' [] = []
k'' [x] = []
k'' (x:xs) = x:k'' xs

k :: [[a]] -> [[a]]
k [x] = [i x x]
k (x1:x2:xs) = k''(i x1 x2 : k (x2:xs)) ++ [i (k'(x1:x2:xs)) x1]

prop_jk :: [String] -> Property
prop_jk xs = (filter (not.null) xs == xs && not(null xs)) ==> j xs == k xs

-- Question 3

data Wff = X
         | Y
         | Tr
         | Fa
         | Not Wff
         | Wff :&: Wff
         | Wff :|: Wff
         | Wff :->: Wff
  deriving (Eq, Show)

instance Arbitrary Wff where
  arbitrary = sized gen
    where
    gen 0 =
      oneof [ return X,
              return Y,
              return Tr,
              return Fa ]
    gen n | n>0 =
      oneof [ return X,
              return Y,
              return Tr,
              return Fa,
              liftM Not wff,
              liftM2 (:&:) wff wff,
              liftM2 (:|:) wff wff,
              liftM2 (:->:) wff wff]
      where
      wff = gen (n `div` 2)

-- 3a

eval :: Bool -> Bool -> Wff -> Bool
eval x y (a :->: b) = not(eval x y a) || eval x y b
eval x y (a :&: b)  = eval x y a && eval x y b
eval x y (a :|: b)  = eval x y a || eval x y b
eval x y (Not a)    = not(eval x y a)
eval x y X          = x
eval x y Y          = y
eval x y Tr         = True
eval x y Fa         = False

prop_eval :: Bool -> Bool -> Wff -> Bool
prop_eval x y wff = eval x y wff == eval x y (simplify wff)

-- 3b

simple :: Wff -> Bool
simple Tr = True
simple Fa = True
simple X  = True
simple Y  = True
simple (a :->: b) = (a /= Tr && a /= Fa && b /= Tr && b /= Fa)
                    && (simple a && simple b)
simple (a :&: b)  = (a /= Tr && a /= Fa && b /= Tr && b /= Fa)
                    && (simple a && simple b)
simple (a :|: b)  = (a /= Tr && a /= Fa && b /= Tr && b /= Fa)
                    && (simple a && simple b)
simple (Not a)    = (a /= Tr && a /= Fa) && simple a

-- 3c

simplify' :: Wff -> Wff
simplify' Tr = Tr
simplify' Fa = Fa
simplify' X  = X
simplify' Y  = Y

simplify' (Not Tr) = Fa
simplify' (Not Fa) = Tr

simplify' (Tr :|: p) = Tr
simplify' (p :|: Tr) = Tr

simplify' (Fa :&: p) = Fa
simplify' (p :&: Fa) = Fa

simplify' (Fa :->: p) = Tr
simplify' (p :->: Tr) = Tr

simplify' (Tr :&: p) = simplify' p
simplify' (p :&: Tr) = simplify' p

simplify' (Fa :|: p) = simplify' p
simplify' (p :|: Fa) = simplify' p

simplify' (Tr :->: p) = simplify' p
simplify' (p :->: Fa) = Not(simplify' p)

simplify' (a :->: b) = simplify' a :->: simplify' b
simplify' (a :&: b)  = simplify' a :&: simplify' b
simplify' (a :|: b)  = simplify' a :|: simplify' b
simplify' (Not a)    = Not(simplify' a)

simplify :: Wff -> Wff
simplify a | not(simple(simplify' a)) = simplify(simplify' a)
           | otherwise = simplify' a

prop_simplify :: Wff -> Bool
prop_simplify wff = simple(simplify wff)

