module Tutorial6 where

import Data.List (nub, delete, sortOn)
--import Data.Sort(uniqueSort, sort)
import Data.Maybe  (mapMaybe)
import Control.Monad( liftM, liftM2 )
import Debug.Trace
import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )


-- ** Warmup exercises

-- The datatype 'Fruit'

data Fruit = Apple String Bool
           | Orange String Int

-- Some example Fruit
apple, apple', orange :: Fruit
apple  = Apple "Granny Smith" False -- a Granny Smith apple with no worm
apple' = Apple "Braeburn" True     -- a Braeburn apple with a worm
orange = Orange "Sanguinello" 10    -- a Sanguinello with 10 segments

fruits :: [Fruit]
fruits = [Orange "Seville" 12,
          Apple "Granny Smith" False,
          Apple "Braeburn" True,
          Orange "Sanguinello" 10]

-- This allows us to print out Fruit in the same way we print out a list, an Int or a Bool.
instance Show Fruit where
  show (Apple variety hasWorm)   = "Apple("  ++ variety ++ "," ++ show hasWorm  ++ ")"
  show (Orange variety segments) = "Orange(" ++ variety ++ "," ++ show segments ++ ")"

-- 1.
isBloodOrange :: Fruit -> Bool
isBloodOrange (Orange str n)
  | str == "Tarocco" || str == "Moro" || str == "Sanguinello" = True
  | otherwise = False
isBloodOrange _ = False


-- 2.
bloodOrangeSegments :: [Fruit] -> Int
bloodOrangeSegments xs = sum([segments | (Orange variety segments) <- (filter isBloodOrange xs)])

-- 3.
isApple :: Fruit -> Bool
isApple (Apple str hasWorm) = True
isApple _ = False

worms :: [Fruit] -> Int
worms fs = length([Apple | (Apple variety hasWorm) <- (filter isApple fs), hasWorm])


-- ** Implementing propositional logic in Haskell

-- The datatype 'Wff' a
data Wff a = V a
           | T
           | F
           | Not (Wff a)
           | Wff a :|: Wff a
           | Wff a :&: Wff a
           | Wff a :->: Wff a
           | Wff a :<->: Wff a
           deriving (Eq, Ord)
infixr 3 :&:
infixr 2 :|:
infixr 1 :->:
infixr 0 :<->:

 
data Atom = A|B|C|D|P|Q|R|S|W|X|Y|Z deriving (Eq, Show, Ord)
-- we will use these as propositional letters in examples         
type Env a = [(a, Bool)]

lookUp :: Eq a => Env a -> a -> Bool
lookUp v x = the [ b | (x', b) <- v, x == x' ]
    where the [z] = z
          the []  = error ("valuation undefined")
          the zs  = error ("multiple values")
-- we represent valuations abstractly as functions.
-- The code above generates such a funcion from an association list.


-- Functions for handling Wffs
-- Use a function to substitute values for atoms
substitute :: (a -> b) -> Wff a -> Wff b
substitute _ T         = T
substitute _ F         = F
substitute f (Not p)   = Not (substitute f p)
substitute f (p :|: q) = substitute f p :|: substitute f q
substitute f (p :&: q) = substitute f p :&: substitute f q
substitute f (p :->: q) = substitute f p :->: substitute f q
substitute f (p :<->: q) = substitute f p :<->: substitute f q 
substitute f (V a)     = V (f a)

-- evaluate a Wff whose atoms are Booleans
evaluate :: Wff Bool -> Bool 
evaluate T         = True
evaluate F         = False
evaluate (Not p)   = not (evaluate p)
evaluate (p :&: q) = evaluate p && evaluate q
evaluate (p :|: q) = evaluate p || evaluate q
evaluate (p :->: q) = if evaluate p && evaluate (Not q) then False else True
evaluate (p :<->: q) = if evaluate p == evaluate q then True else False
evaluate (V b)     = b

-- evaluates a wff in a given environment
eval :: Eq a => Env a -> Wff a -> Bool
eval v wff = evaluate ( substitute (lookUp v) wff )

-- list the atoms that occur in a wff - 
--  NOTE: atoms in the result must be unique
atoms :: Eq a => Wff a -> [a]
atoms (V x)     = [x]
atoms (F)       = []
atoms (T)       = []
atoms (Not p)   = atoms p
atoms (p :|: q) = nub (atoms p ++ atoms q)
atoms (p :&: q) = nub (atoms p ++ atoms q)
atoms (p :->: q) = nub (atoms p ++ atoms q)
atoms (p :<->: q) = nub (atoms p ++ atoms q)

-- creates all possible truth assignments for a set of atoms
envs :: [a] -> [Env a]
envs []     = [[]]
envs (x:xs) = [ (x,False):e | e <- envs xs ] ++
                [ (x,True ):e | e <- envs xs ]

-- checks whether a wff is satisfiable
satisfiable :: Eq a => Wff a -> Bool
satisfiable p = or [ eval e p | e <- envs (atoms p) ]

-- models :: Eq a => Wff a -> [Env a]
-- models p = [ e | e <- envs (atoms p),  eval e p ]


-- ** Exercises

-- 4.
wff1 = ((V P :|: V Q) :&: (V P :&: V Q))
-- satisfiable wff1 == True
-- P Q | (P | Q) & P & Q
-- - - | ---------------
-- F F |        F
-- F T |        F       
-- T F |        F
-- T T |        T       

wff2 = (V P :&: (V Q :|: V R)) :&: ((Not(V P) :|: Not(V Q)) :&: (Not(V P) :|: Not(V R)))
-- satisfiable wff2 == False
-- P Q R | P & (Q | R) & (~P | ~Q) & (~P | ~R)
-- - - - | -----------------------------------
-- F F F |                  F
-- F F T |                  F
-- F T F |                  F
-- F T T |                  F
-- T F F |                  F
-- T F T |                  F
-- T T F |                  F
-- T T T |                  F

-- 5. 
tautology :: Eq a => Wff a -> Bool
tautology wff = null[x | x <- envs(atoms(wff)), not(eval x wff)] 

-- i.  either P is a tautology, or ~P is satisfiable

prop_taut1 :: Wff Atom -> Bool
prop_taut1 wff = (tautology wff) /= (satisfiable(Not(wff)))

-- ii.  either P is not satisfiable, or ~P is not a tautology
prop_taut2 :: Wff Atom -> Bool
prop_taut2 wff = not(tautology (Not(wff))) /= not(satisfiable(wff))

-- Any other properties you can think of?
prop_taut :: Wff Atom -> Bool
prop_taut wff = not(tautology (wff)) /= (tautology (wff))

-- 6.
wff3 = ((V P :->: V Q) :&: (V P :&: Not(V Q)))
-- satisfiable wff3 == False
-- P Q | (P -> Q) & P & ~Q
-- - - | -----------------
-- F F |         F        
-- F T |         F
-- T F |         F
-- T T |         F        

wff4 = ((V P :<->: V Q) :&: ((V P :&: Not(V Q)) :|: (Not(V P) :&: V Q)))
-- satiafiable wff4 == False
-- P Q | (P <-> Q) & (P & ~Q | ~P & Q)
-- - - | -----------------------------
-- F F |               F
-- F T |               F
-- T F |               F
-- T T |               F

-- 7.
equivalent :: Eq a =>  Wff a -> Wff a -> Bool
equivalent wffA wffB = [eval env wffA | env <- envs(nub(atoms(wffA) ++ atoms(wffB)))] == [eval env wffB | env <- envs(nub(atoms(wffA) ++ atoms(wffB)))]


-- 8.
subformulas :: Eq a => Wff a -> [Wff a]
subformulas (V x)     = [(V x)]
subformulas (Not p)   = [Not(p)] ++ subformulas p
subformulas (p :|: q) = [(p :|: q)] ++ nub (subformulas p ++ subformulas q)
subformulas (p :&: q) = [(p :&: q)] ++ nub (subformulas p ++ subformulas q)
subformulas (p :->: q) = [(p :->: q)] ++ nub (subformulas p ++ subformulas q)
subformulas (p :<->: q) = [(p :<->: q)] ++ nub (subformulas p ++ subformulas q)

-- 9.
wff5 = ((V P :|: V Q) :&: (Not(V P) :&: Not(V Q)))
-- satisfiable wff5 == False
-- P Q | (P | Q) & ~P & ~Q
-- - - | -----------------
-- F F |         F
-- F T |         F        
-- T F |         F
-- T T |         F        
-- tautology wff5 == False
-- tautology (Not(wff5)) == True

wff6 = ((V P :->: V Q) :&: (V P :<->: V Q))
-- satisfiable wff6 == True
-- P Q | (P -> Q) & (P <-> Q)
-- - - | --------------------
-- F F |          T
-- F T |          F
-- T F |          F
-- T T |          T

equivalent' :: Eq a => Wff a -> Wff a -> Bool
equivalent' wffA wffB = tautology (wffA :<->: wffB)

prop_equivalent :: Wff Atom -> Wff Atom -> Bool
prop_equivalent wffA wffB = equivalent wffA wffB == equivalent' wffA wffB


-- ** Optional Material

-- 10.
-- check for negation normal form
isNNF :: Wff a -> Bool
isNNF (p :|: q) = isNNF (p) && isNNF (q)
isNNF (p :&: q) = isNNF (p) && isNNF (q)
isNNF (Not(p :|: q)) = False
isNNF (Not(p :&: q)) = False
isNNF (Not(V _)) = True
isNNF (V _) = True
isNNF T = True
isNNF F = True
isNNF (p :->: q) = False
isNNF (p :<->: q) = False
isNNF (Not(p)) = False

-- 11.
-- convert to negation normal form
impElim :: Wff a -> Wff a
impElim w = w
impElim (Not w) = Not(impElim w)
impElim (p :|: q) = impElim(p) :|: impElim (q)
impElim (p :&: q) = impElim(p) :&: impElim (q)
impElim (p :->: q) = (Not (impElim(p)) :|: impElim(q))
impElim (p :<->: q) = impElim((p :->: q) :&: (q :->: p))

toNNF :: Wff a -> Wff a
toNNF (V a) = V a
toNNF Not(Not(p)) = p
toNNF (Not (p :|: q)) = toNNF(Not p) :&: toNNF(Not q)
toNNF (Not (p :&: q)) = toNNF(Not p) :|: toNNF(Not q)
toNNf (Not(p :->: q)) = impElim(p :&: Not(q))

-- check if result of toNNF is in neg. normal form
prop_NNF1 :: Wff Atom -> Bool
prop_NNF1 f = isNNF (toNNF f)

-- check if result of toNNF is equivalent to its input
prop_NNF2 :: Wff Atom -> Bool
prop_NNF2 f = equivalent f (toNNF f)

-- 12.
-- check whether a formula is in conj. normal form
isCNF :: Eq a => Wff a -> Bool
isCNF (p :&: (q :|: r)) = False
isCNF ((p :|: q) :|: r) = False
isCNF (p :|: (q :|: r)) = False
isCNF (p :|: q) = isCNF (p) && isCNF (q)
isCNF (p :&: q) = isCNF (p) && isCNF (q)
isCNF (Not(p :|: q)) = False
isCNF (Not(p :&: q)) = False
isCNF (Not(V _)) = True
isCNF (V _) = True
isCNF T = True
isCNF F = True
isCNF (p :->: q) = False
isCNF (p :<->: q) = False 

-- 13.
-- transform a list of lists into a (CNF) formula
listsToCNF :: [[Wff a]] -> Wff a
listsToCNF [] = T
listsToCNF [[]] = T
listsToCNF (a:[]) = toNNF(foldr1 (:|:) a)
listsToCNF (a:as) = toNNF(foldr1 (:|:) a) :&: listsToCNF as

-- 14.
-- transform a CNF formula into a list of lists
listsFromCNF :: Eq a => Wff a -> [[Wff a]]
listsFromCNF = undefined
-- listsFromCNF (a :&: b) = [listsFromCNF(a)] ++ [listsFromCNF(b)]
-- listsFromCNF (p :|: q) = listsFromCNF(p) : [listsFromCNF(q)]
-- listsFromCNF p = extraction p
  -- where extraction (V a) = (V a)
       --  extraction T = T
       --  extraction F = F

-- 15.
-- transform an arbitrary formula into a list of lists
toCNFList :: Eq a => Wff a -> [[Wff a]]
toCNFList = undefined

-- convert to conjunctive normal form
toCNF :: Eq a => Wff a -> Wff a
toCNF = listsToCNF . toCNFList

-- check if result of toCNF is in con. normal form
prop_CNF1 :: Wff Atom -> Bool
prop_CNF1 f = undefined

-- check if result of toCNF is equivalent to its input
prop_CNF2 :: Wff Atom -> Bool
prop_CNF2 p = undefined


-- ** PRESENTATION
showWff :: Show a => Wff a -> String
showWff e = showsPrec 0 e ""
  where
    showsPrec _ (V a) = showString (show a)
    showsPrec _  F    = showChar 'F'
    showsPrec _  T    = showChar 'T'
    showsPrec p (a :&: b)
      = showParen (p>3)
         (showsPrec 3 a .showSpace .
          showString "&" . showSpace . 
          showsPrec 3 b )
    showsPrec p (a :|: b)
      = showParen (p>2)
         (showsPrec 2 a .showSpace .
          showString "|" . showSpace . 
          showsPrec 2 b )
    showsPrec p (a :->: b)
      = showParen (p>1)
        (showsPrec 1 a .showSpace .
          showString "->" . showSpace . 
          showsPrec 1 b )
    showsPrec p (a :<->: b)
      = showParen (p>0)
        (showsPrec 0 a .showSpace .
          showString "<->" . showSpace . 
          showsPrec 0 b )
    showsPrec _ (Not a) = 
      showString "~" . showsPrec 11 a
    showString :: String -> String -> String
    showString = (++)
    showChar :: Char -> String -> String
    showChar   = (:)
    showSpace :: String -> String
    showSpace  = showChar ' '
    showParen :: Bool -> (String -> String) -> (String -> String)
    showParen p s = if p then showChar '(' . s . showChar ')' else s


-- ** For Drawing Tables

-- centre a string in a field of a given width
centre :: Int -> String -> String
centre w s = replicate h ' ' ++ s ++ replicate (w-n-h) ' '
            where
            n = length s
            h = (w - n) `div` 2

-- make a string of dashes as long as the given string
dash :: String -> String
dash s = replicate (length s) '-'

-- convert boolean to T or F
fort :: Bool -> String
fort False = "F"
fort True  = "T"

-- print a table with columns neatly centred
-- assumes that strings in first row are longer than any others
showTable :: [[String]] -> IO ()
showTable tab = putStrLn (
  unlines [ unwords (zipWith centre widths row) | row <- tab ] )
  where
    widths  = map length (head tab)

table p = tables [p]

tables :: (Eq a, Show a) => [Wff a] -> IO ()
tables ps  =
  let xs = nub (concatMap atoms ps) in
   showTable (
     [ map show xs ++ ["|"] ++ [show p | p <- ps]           ] ++
     [ dashvars xs ++ ["|"] ++ [dash (show p) | p <- ps ]   ] 
     ++   [ evalvars e xs ++ ["|"] ++ [fort (eval e p) | p <- ps ] | e <- envs xs]
     )
  where  dashvars xs   = [ dash (show x) | x <- xs ]
         evalvars e xs = [ fort (eval e (V x)) | x <- xs ]

 -- print a truth table, including columns for subformulas
fullTable :: (Eq a, Show a) => Wff a -> IO ()
fullTable = tables . filter nontrivial . sortOn (length.atoms) . subformulas
  where nontrivial :: Wff a -> Bool
        nontrivial (Not(V _))   = False
        nontrivial (V _)        = False
        nontrivial T            = False
        nontrivial F            = False
        nontrivial _            = True


-- ** For QuickCheck
 
instance Show a => Show (Wff a) where
  show = showWff

instance Arbitrary Atom where
  arbitrary = oneof $ map return [ A, B, C, D, W, X, Y, Z]

instance Arbitrary a => Arbitrary (Wff a) where
  arbitrary = sized wff
      where
        wff n | n <= 0    = liftM V atom
              | otherwise = oneof [ liftM V atom
                                     , liftM Not subform
                                     , liftM2 (:|:) subform subform
                                     , liftM2 (:&:) subform subform
                                   --  , liftM2 (:->:) subform subform
                                   --  , liftM2 (:<->:)subform' subform'
                                     ]
               where
                 atom = Test.QuickCheck.arbitrary
                 subform = wff (n `div` 2)
                 subform' =  wff (n `div` 4)
