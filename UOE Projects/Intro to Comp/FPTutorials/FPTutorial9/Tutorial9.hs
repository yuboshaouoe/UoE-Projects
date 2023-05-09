module Tutorial9 where

-- Sudoku solver
-- Based on Bird, "Thinking Functionally with Haskell"

import Test.QuickCheck
import Data.List (sort,nub,(\\),transpose,genericLength)
import Data.String (lines,unlines)


type Row a    = [a]
type Matrix a = [Row a]
type Digit    = Char

digits :: [Digit]
digits = ['1'..'9']

blank :: Digit -> Bool
blank d = d == ' '

-- 1.
group :: [a] -> [[a]]
group = groupBy 3

groupBy :: Int -> [a] -> [[a]]
groupBy n []   = []
groupBy n all  = take n all : groupBy n (drop n all)

-- 2.
intersperse :: a -> [a] -> [a]
intersperse sym []     = [sym]
intersperse sym (x:xs) = [sym, x] ++ intersperse sym xs 

-- 3.
showRow :: String -> String
showRow str = concat $ intersperse "|" (group str)

-- 4.
showGrid :: Matrix Digit -> [String]
showGrid xs = concat $ intersperse ["-------------"] (group xs)

-- 5.
put :: Matrix Digit -> IO ()
put xs = putStrLn $ unlines (showGrid $ map showRow xs)

-- 6.
repl :: Char -> Char
repl ' ' = '.'
repl  c  =  c

showMat :: Matrix Digit -> String
showMat mx = map repl (concat(mx))
 
repl' :: Char -> Char
repl' '.' = ' '
repl'  c  =  c

readMat :: String -> Matrix Digit
readMat str = groupBy 9 (map repl' str)

-- 7.

repl'' :: Char -> [Char]
repl'' ' ' = "123456789"
repl''  c  = [c]

choices :: Matrix Digit -> Matrix [Digit]
choices xs = [map repl'' x | x <- xs]

-- 8.
cp :: [[a]] -> [[a]]
cp []       = [[]]
cp (xs:xss) = [ x:ys | x <- xs, ys <- cp xss ]

prop_cp :: [[a]] -> Bool
prop_cp xss = length (cp xss) == product (map length xss)
-- slow: use `quickCheckWith (stdArgs {maxSize = 5})`

expand :: Matrix [Digit] -> [Matrix Digit]
expand []     = []
expand (x:xs) = [groupBy (maximum (map length x)) a | a <- (cp $ concat(x:xs))]

-- 9.
prop_expand :: Matrix [Digit] -> Bool
prop_expand [] = True
prop_expand m = length(expand m) == product[product(map length x) | x <- m]

-- 10.
easySols :: Integer
easySols = fromIntegral $ length $ expand $ choices easy

-- 11, 12, 13.
rows, cols, boxs:: Matrix a -> Matrix a
rows a = a
cols = transpose
boxs a = map concat (concat(map cols (group(map group a))))

-- 14.
distinct :: Eq a => [a] -> Bool
distinct xs = nub xs == xs

-- 15.
valid :: Matrix Digit -> Bool
valid g = all distinct (rows g) && all distinct (cols g) && all distinct (boxs g)

-- 16.
simple :: Matrix Digit -> [Matrix Digit]
simple = filter valid . expand . choices
{- No, because it would take forever to compute the solution, 
   but it's possible, given enough time.
-}

-- 17.
the :: [Digit] -> Digit
the [d] = d

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = map remove row
        where single x = x `elem` [n | n <- row, length n == 1]
              remove y = if not(single y) 
                         then [m | m <- y, not(single [m])]
                         else y

-- 18.
pruneBy :: (Matrix [Digit] -> Matrix [Digit]) -> Matrix [Digit] -> Matrix [Digit]
pruneBy f = f . map pruneRow . f

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy rows . pruneBy cols . pruneBy boxs 

-- 19.
many :: Eq a => (a -> a) -> a -> a
many f xs = head[iterate f xs !! n | n <- [0..], iterate f xs !! n == iterate f xs !! (n+1), iterate f xs !! n /= iterate f xs !! (n-1)]

close :: (Eq a, Ord a) => [(a,a)] -> [(a,a)]
close pairs = nub (sort (pairs ++
                [ (x,z) | (x,y) <- pairs,
                (y',z) <- pairs,
                y == y' ]))

-- 20.
extract :: Matrix [Digit] -> Matrix Digit
extract = map concat

-- 21.
solve :: Matrix Digit -> Matrix Digit
solve a = extract $ many prune $ choices a 


-- ** Optional Material

-- 22.
failed :: Matrix [Digit] -> Bool
failed mat = not $ and [ and [ not(null b) | b <- a ] | a <- mat ]

-- 23.
solved :: Matrix [Digit] -> Bool
solved mat = and [ and [ length b == 1 | b <- a ] | a <- mat ]

-- 24.
shortest :: Matrix [Digit] -> Int
shortest mat = minimum [ minimum [ length b | b <- a, length b > 1 ] | a <- mat ]

-- 25.
-- earliest :: Matrix [Digit] -> Row [Digit]
-- earliest mat = mat !! index
        -- where lst = [ minimum [ length b | b <- a, length b > 1 ] | a <- mat ]
              -- index = head [ n | n <- [0..length lst-1], lst !! n == shortest mat ]

expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 mat = [ preMat ++ [preRow ++ [[n]] ++ postRow] ++ postMat | n <- ds]
                 where (preMat, row:postMat) = break (\row -> not $ null[ n | n <- row ,length n == shortest mat]) mat
                       (preRow, ds:postRow) = break (\ds -> length ds == shortest mat) row

-- 26.
search :: Matrix Digit -> [Matrix Digit]
search xs  = [extract $ many prune n| n <- helper xs, solved (many prune n), not $ failed $ many prune n]
                 where helper xs = concat[expand1 (many prune n) | n <- expand1 (many prune (choices xs))]

test :: Matrix Digit
test = ["29574386 ",
        "43186 927",
        "8 6192543",
        "387459216",
        "6123 7495",
        "549216738",
        "7 3524189",
        "928671 54",
        "15 938672"]

test' :: Matrix [Digit]
test' = [["9","1578","3","67","167","4","2","15678","5678"],
        ["4","178","6","5","12379","127","138","13789","789"],
        ["57","157","2","8","13679","167","135","1345679","5679"],
        ["238","238","9","267","2678","5","138","13678","4"],
        ["58","6","7","1","4","3","9","2","58"],
        ["1","23458","45","9","2678","267","358","35678","5678"],
        ["2356","123459","145","246","1256","8","7","59","259"],
        ["2567","1257","15","267","12567","9","4","58","3"],
        ["257","24579","8","3","257","27","6","59","1"]]


-- Example from Bird

book :: Matrix Digit
book = ["  4  57  ",
        "     94  ",
        "36      8",
        "72  6    ",
        "   4 2   ",
        "    8  93",
        "4      56",
        "  53     ",
        "  61  9  "]

-- Examples from websudoku.com

easy :: Matrix Digit
easy = ["    345  ",
        "  89   3 ",
        "3    2789",
        "2 4  6815",
        "    4    ",
        "8765  4 2",
        "7523    6",
        " 1   79  ",
        "  942    "]

medium :: Matrix Digit
medium = ["   4 6 9 ",
          "     3  5",
          "45     86",
          "6 2 74  1",
          "    9    ",
          "9  56 7 8",
          "71     64",
          "3  6     ",
          " 6 9 2   "]

hard :: Matrix Digit
hard = ["9 3  42  ",
        "4 65     ",
        "  28     ",
        "     5  4",
        " 67 4 92 ",
        "1  9     ",
        "     87  ",
        "     94 3",
        "  83  6 1"]

evil :: Matrix Digit
evil = ["  9      ",
        "384   5  ",
        "    4 3  ",
        "   1  27 ",
        "2  3 4  5",
        " 48  6   ",
        "  6 1    ",
        "  7   629",
        "     5   "]

puts :: [Matrix Digit] -> IO ()
puts = sequence_ . map put

puzzle :: Matrix Digit -> IO ()
puzzle g = put g >> puts (search g) >> putStrLn "***"
       
main :: IO ()
main = puzzle easy >>
       puzzle medium >>
       puzzle hard >>
       puzzle evil

