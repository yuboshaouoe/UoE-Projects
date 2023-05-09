module Tutorial8 where

import System.Random

-- Importing the keymap module

import KeymapTree


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen [] = 0
longestProductLen (a:as) = max (length(fst(snd(a)))) (longestProductLen(as))
-- longestProductLen [(b,(n,c))] = maximum([length(n) | (b,(n,c)) <- ([(b,(n,c))])])

formatLine :: Int -> (Barcode, Item) -> String
formatLine len (b,(name, cata))
  | len-length(name) < 0 = b ++ "..." ++ take len name ++ "..." ++ cata
  | otherwise            = b ++ "..." ++ take len name ++ 
                           replicate (len-length(name)+3) '.'
                           ++ cata
                                
showCatalogue :: Catalogue -> String
showCatalogue x = concat[(formatLine (longestProductLen(toList(x))) (b,(n,c))) ++ "\n" | (b,(n,c)) <- (toList(x))]

-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList a = case a of
  Nothing -> []
  Just a -> [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe [a] = Just a

catMaybes :: [Maybe a] -> [a]
catMaybes xs = concat(map maybeToList xs) 

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems bs c = catMaybes([get b c | b <- bs])


-- Exercise 4

-- a) 1.43 secs
-- b) 3.75 secs over 100 items => 0.0375 per item
-- c) size theDB == 104651, thus if the target item is the last one
--    in the data base, it would take 104651*0.0375 == 3924.4125 secs
--    to find the target item. Double the total items in theDB would 
--    double to time used on average to find the target item.

-- Exercise 5
-- *Tutorial8> size ( Node "0001" "just some item" Leaf Leaf )
-- <interactive>:12:42: error: Data constructor not in scope: Leaf
-- Not quite sure about why, I have changed KeymapList to KeymapTree.

-- For Exercises 6-10 check KeymapTree.hs 

-- Exercise 11

-- a) 9.80 secs, 2,043,715,480 bytes
-- b) n at most, log(n) in average.

-- ** Inp)ut-output

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)

samples :: Int -> Catalogue -> IO [Barcode]
samples n db = sequence [getSample db | i <- [1..n]]

gets :: [Barcode] -> Catalogue -> [Item]
gets ks db  =  [x | k <- ks, Just x <- [get k db]]
