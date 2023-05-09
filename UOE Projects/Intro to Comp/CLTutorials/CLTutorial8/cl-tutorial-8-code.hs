import Data.List
import Data.Char(intToDigit)
                       
-- [ The following lines code our language of forms ]
    
data Literal atom = P atom | N atom
                    deriving (Eq, Show)

data Clause atom = Or [Literal atom]
                   deriving (Eq, Show)

data Form atom = And [Clause atom]
                 deriving (Eq, Show)

neg :: Literal atom -> Literal atom
neg (P a) = N a
neg (N a) = P a                          

(<&&>) :: Form a -> Form a -> Form a
And xs <&&> And ys = And ( xs ++ ys )

-- | The next three functions correspond to the optimized
-- implementation of the DPLL algorithm, as seen in the textbook
-- on page 184.
                    
(<<) :: Eq atom => [Clause atom] -> Literal atom -> [Clause atom]
cs << l = [ Or (delete (neg l) ls)
               | Or ls <- cs, not (l `elem` ls) ]

dpll :: Eq atom => Form atom -> [[Literal atom]]
dpll f =
    case prioritise f of
      [] -> [[]] -- the trivial solution
      Or [] : cs -> [] -- no solution
      Or (l:ls) : cs ->
          [ l : ls | ls <- dpll (And (cs << l)) ]
          ++
          [ neg l : ls | ls <- dpll (And (Or ls : cs << neg l)) ]

prioritise :: Form atom -> [Clause atom]
prioritise (And cs) = sortOn (\(Or ls) -> length ls) cs

-- [ Now we start playing Killer Sudoku ]
-- 
-- We follow the description in the textbook on pages 185–187.

sudoku :: Form (Int, Int, Int)
sudoku = allFilled <&&> noneFilledTwice
         <&&> rowsComplete <&&> columnsComplete <&&> squaresComplete
         <&&> rowsNoRepetition <&&> columnsNoRepetition <&&> squaresNoRepetition

allFilled :: Form (Int,Int,Int)
allFilled = And [ Or [ P (i,j,n) | n <- [1..9] ]
                | i <- [1..9], j <- [1..9] ]

noneFilledTwice :: Form (Int,Int,Int)
noneFilledTwice = And [ Or [ N (i, j, n), N (i, j, n') ]
                      | i <- [1..9], j <- [1..9],
                        n <- [1..9], n' <- [1..(n-1)]]

rowsComplete :: Form (Int,Int,Int)
rowsComplete = And [ Or [ P (i, j, n) | j <- [1..9] ]
                   | i <- [1..9], n <- [1..9] ]

columnsComplete :: Form (Int,Int,Int)
columnsComplete = And [ Or [ P (i, j, n) | i <- [1..9] ]
                      | j <- [1..9], n <- [1..9] ]

squaresComplete :: Form (Int,Int,Int)
squaresComplete = And [ Or [ P (3*p+q, 3*r+s, n)
                           | q <- [1..3], s <- [1..3] ]
                      | p <- [0..2], r <- [0..2], n <- [1..9] ]

rowsNoRepetition :: Form (Int,Int,Int)
rowsNoRepetition = And [ Or [ N (i, j, n), N (i, j', n) ]
                       | i <- [1..9], n <- [1..9],
                         j <- [1..9], j' <- [1..(j-1)] ]

columnsNoRepetition :: Form (Int,Int,Int)
columnsNoRepetition = And [ Or [ N (i, j, n), N (i', j, n) ]
                          | j <- [1..9], n <- [1..9], i <- [1..9], i' <- [1..(i-1)] ]
                      
squaresNoRepetition :: Form (Int,Int,Int)
squaresNoRepetition = And [ Or [ N (3*p+q, 3*r+s, n), N (3*p+q', 3*r+s', n) ]
                          | p <- [0..2], r <- [0..2], n <- [1..9],
                            q <- [1..3], s <- [1..3], q' <- [1..q], s' <- [1..3],
                            q' < q || s' < s ]

solutions :: Form (Int, Int, Int) -> [[Literal (Int, Int, Int)]]
solutions problem = dpll (sudoku <&&> problem)

-- | A sample Sudoku problem. You may replace this with your own problem.

sudokuProblem :: Form (Int, Int, Int)
sudokuProblem = And [ Or [P (1,8,8)], Or [P (1,9,2)], Or [P (2,1,6)]
                    , Or [P (2,4,4)], Or [P (4,1,4)], Or [P (4,5,7)]
                    , Or [P (4,6,2)], Or [P (5,1,5)], Or [P (5,7,4)]
                    , Or [P (5,8,3)], Or [P (6,5,1)], Or [P (7,4,8)]
                    , Or [P (7,7,6)], Or [P (8,2,8)], Or [P (8,3,1)]
                    , Or [P (9,2,2)], Or [P (9,9,7)]]
                
-- [ Pretty printing for Sudoku problems and solutions ]
--
-- The following (optional) functions give you nice and easy to read
-- representations of Sudoku problems and solutions. We do not expect
-- you to understand their definitions.
-- 
-- You may find them useful to check that sudokuProblem is a correct
-- encoding of the problem you are trying to solve, and also to
-- visualise the solutions.
--
-- There are three functions you may want to use:
-- 
-- printProblem sudokuProblem
--   pretty prints sudokuProblem
--
-- printAllSolutions sudokuProblem
--   pretty prints all solutions to sudokuProblem
--   (please note that this may take a while)
-- 
-- printSolution . head . solutions $ sudokuProblem
--   pretty prints the first solution to sudokuProblem
--   (please note that this may take a while)

-- toLiterals should be used with Forms that represent problems given
-- by a collection of positive unit clauses.
toLiterals :: Form atom -> [Literal atom]
toLiterals (And clauses) = concat $ map unpack clauses
    where unpack (Or literals) = literals
                                 
showSquares :: [Literal (Int,Int,Int)] -> String
showSquares lits =
  let pos = [ a | P a <- lits ]
  in
   [ (intToDigit.last) [ k | k <-[0..9]
                       , (i, j, k)`elem`pos || k == 0 ]
   | i <- [1..9], j <- [1..9] ]
  
-- | pretty takes an 81 digit string and presents it in sudoku form
-- using unicode -- suitable for putStrLn
pretty :: String -> String
pretty = ((tl++dsh++dn++dsh++dn++dsh++tr++"\n"++vt++" ")++)
         . (++(" "++vt++" \n"++bl++dsh++up++dsh++up++dsh++br))
         . intercalate (" "++vt++"\n"++vl++dsh++pl++dsh++pl++dsh++vr++" \n"++vt++" ")
         . map (intercalate (" "++vt++"\n"++vt++" ")) . byThree
         . map (intercalate (" "++vt++" ")). byThree
         . map (intersperse ' ')  . byThree
         . map (\d -> if d == '0' then '\x005F' else d)
  where
    byThree :: [a] -> [[a]]
    byThree (a : b : c : xs) = [a,b,c] : byThree xs
    byThree [] = []
    tl = "\x250F" -- topleft
    tr = "\x2513" -- topright
    bl = "\x2517" -- botleft
    br = "\x251B" -- botright
    dn = "\x2533"
    up = "\x253B"
    vl = "\x2523" -- vertleft
    vr = "\x252B" -- vertright
    vt = "\x2503" -- vertical
    pl = "\x254B" -- plus
    dsh = take 7 $ repeat '\x2501'
  
printProblem :: Form (Int, Int, Int) -> IO ()
printProblem = putStrLn . pretty . showSquares . toLiterals

printSolution :: [Literal (Int, Int, Int)] -> IO ()
printSolution = putStrLn . pretty . showSquares

printAllSolutions :: Form (Int, Int, Int) -> IO ()
printAllSolutions = mapM_ printSolution . solutions

-- [ Your contribution starts here ]

-- | Write a function `scores` that takes two natural numbers
-- (non-negative integers), say n and m, and returns the list of
-- all lists ds of digits from [1..9] such that:
--
-- 1. length ds == n;
-- 2. sum ds ==  m.
combinations :: Int -> [Int] -> [[Int]]
combinations k ns = filter ((k==).length) $ subsequences ns

allcombinations :: Int -> [Int] -> [[Int]]
allcombinations 0 xs = []
allcombinations _ [] = []
allcombinations k xs = combinations k xs ++ allcombinations (k-1) xs

scores :: Int -> Int -> [[Int]]
scores n m = [ds | ds <- (allcombinations 9 ([1..9])), length(ds) == n, sum(ds) == m]

-- | The shapes for Killer Sudoku are represented as lists of cells.
type Shape = [(Int, Int)]

-- | Given a shape sh and a list of scores ss (of the same length),
-- we combine them to give a list of triples (a list of atoms).
--
-- The name of the function (>>*<) comes from searching for a
-- function with the type [(a, b)] -> [c] -> [(a, b, c)] on
-- Hoogle. Using accepted names where they exist is in general
-- good practice.
(>>*<) :: [(a, b)] -> [c] -> [(a, b, c)]
sh >>*< ss = [ (i, j, k) | ((i,j), k) <- zip sh ss ]

-- | To say that a given list aa of atoms may not be part of the
-- solution we add a clause for "at least one of the atoms is false".
deny :: [atom] -> Clause atom
deny aa = Or [ N a | a <- aa ]

-- | We cannot say directly in CNF that some pattern of scores that
-- sum to k must occur, but we can deny each pattern of scores that
-- sums to anything other than k.
--
-- Write a function `mustSumTo` that takes an integer k and a
-- shape sh and produces a Form that rejects all patterns of
-- scores whose sum is not k.
notScores :: Int -> Int -> [[Int]]
notScores n m = [ds | ds <- (allcombinations 9 ([1..9])), length(ds) == n, sum(ds) /= m]

mustSumTo :: Int -> Shape -> Form (Int,Int,Int)
mustSumTo k sh = And(listOfImpossibleShapes)
    where listOfImpossibleShapes = [ deny (sh >>*< as) | as <- (notScores (length(sh)) (k))]
  
-- | We call a 'Killer constraint' a list of numbers such that:
--
-- 1. the head of the list represents a sum constraint;
-- 2. the tail of the list corresponds to a shape; each number in
--    the tail of the list represents a position in the Sudoku
--    square: it is the number of a cell; cells are numbered from
--    1 to 81, row by row.
easyKillerCode =
  [ [6,1,2,3], [16,4,5], [5,6], [7,7,16], [13,8,17], [7,9,18],
    [17,10,19], [7,11], [6,12,13], [14,14,15],
    [11,20,21], [1,22], [4,23,32], [7,24,33], [9,25,26], [9,27],
    [12,28,29], [6,30], [9,31,40], [6,34,43], [17,35,36],
    [3,37,38], [17,39,48], [8,41], [16,42,51], [9,44,45],
    [7,46,47], [9,49,58], [7,50,59], [5,52], [8,53,54],
    [6,55], [15,56,57], [2,60], [13,61,62], [3,63,72],
    [7,64,73], [13,65,74], [4,66,75], [12,67,68], [9,69,70], [7,71],
    [7,76], [10,77,78], [19,79,80,81]
  ]

killerCode =
  [ [14,1,2,10], [2,3], [20,4,5,6], [17,7,15,16], [6,8,9],
    [17,11,12], [16,13,14,22,23], [10,17,26], [7,18],
    [12,19,28], [17,20,21,29], [18,24,32,33,42], [16,25,34], [2,27],
    [11,30,38,39], [13,31,40,41], [13,35,36,45],
    [11,37,46,47], [16,43,44,53],
    [14,48,49], [18,50,51,59], [11,52,61,70], [9,54,63],
    [6,55], [15,56,57,58], [11,60,68,69], [15,62,71],
    [11,64,65,66], [12,67,75,76], [18,72,80,81],
    [13,73,74], [13,77,78], [1,79]
  ]

-- | The function `toCell` takes a 1-to-81 position in the Sudoku
-- square and returns its corresponding cell.
toCell :: Int -> (Int,Int)
toCell c = (x+1, y+1) where (x,y) = (c-1) `divMod` 9

-- | The following two functions are auxiliary.
-- You can use them to check that your Killer problem contains all
-- positions of a Sudoku square and that the sum constraints add
-- up to sum of all numbers in a complete Sudoku square.
checkEveryCell ::[[Int]] -> Bool
checkEveryCell kk = (sort . concat) (map tail kk) == [1..81]
                    
checkSum ::[[Int]] -> Bool                    
checkSum kk = sum (map head kk) == 9 * sum [1..9]
                                    

killerClause :: [Int] -> Form (Int,Int,Int)
killerClause (ksum : ss) = mustSumTo ksum (map toCell ss)

killerProblem = foldr (<&&>) sudoku (map killerClause killerCode)
easyKillerProblem =  foldr (<&&>) sudoku (map killerClause easyKillerCode)

-- | To pretty print the solutions, you can run in ghci:
-- 
-- printSolution (head $ dpll easyKillerProblem)
-- 
-- and
-- 
-- printSolution (head $ dpll killerProblem)
