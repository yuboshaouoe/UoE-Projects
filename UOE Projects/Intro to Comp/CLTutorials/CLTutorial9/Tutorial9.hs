-- Informatics 1 - Introduction to Computation
-- Computation and Logic Tutorial 9
--
-- Week 9 (16-22 Nov.)
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, our solutions
-- require only a few of lines for each answer. If your code is getting complicated
-- you're making things too difficult for yourself---try to keep it simple!

module Tutorial9 where
import Prelude hiding (lookup)
import Data.Set(Set, insert, empty, member, fromList, toList,
                 union,intersection, size, singleton, (\\))
import qualified Data.Set as S
import Test.QuickCheck
import Data.Char

-- you should also familiarise yourself with
-- the functions from Data.Set imported above
-- we define infix version of union and intersection
(\/) :: Ord a => Set a -> Set a -> Set a
(\/)  = union
(/\) :: Ord a => Set a -> Set a -> Set a
(/\) = intersection

-- Data.Set provides functions like map and filter that work for sets instead of 
-- lists: we give the names mapS and filterS to the Set versions of map and filter
mapS :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
mapS = S.map
filterS :: Ord a => (a -> Bool) ->  Set a -> Set a
filterS = S.filter

-- Type declarations

type Sym = Char
type Trans q = (q, Sym, q)
--  FSM states symbols transitions starting accepting 
--                   Q      Sigma        delta       S        F
data FSM q = FSM (Set q) (Set Sym) (Set(Trans q)) (Set q) (Set q) deriving Show
mkFSM :: Ord q => [q] -> [Sym] -> [Trans q] -> [q] -> [q] -> FSM q 
mkFSM qs as ts ss fs =  -- a convenience function constructing FSM from lists
  FSM (fromList qs) (fromList as) (fromList ts) (fromList ss) (fromList fs)

-- eg 1
eg1 = mkFSM [0..8] "abcdeghiot"
  [(0,'d',2),(0,'b',6),(1,'h',5),(2,'i',5),(2,'o',8),(3,'t',0),(3,'t',1),(4,'e',1)
  ,(5,'c',4),(5,'o',8),(6,'c',7),(6,'e',8),(7,'a',3),(8,'d',4),(8,'g',7)]
  [0,6] [1,7]

-- Q 6.
-- DFA -- 
isDFA :: Ord q => FSM q -> Bool
isDFA (FSM qs as ts ss fs) = (size ss == 1)
  && and [ length[ q' | q' <- toList qs, (q, a, q')`member`ts ] == 1
               | q <- toList qs, a <- toList as ]
  
-- applying transitions for a given symbol to move a list of states
transition :: (Ord q) => Set(Trans q)  -> Set q -> Sym -> Set q
transition ts qs s  = fromList [ q' | (q, t, q') <- toList ts, t == s, q `member` qs ]

-- applying transitions for a string of symbols
-- final ::  Ord q => Set(Trans q) ->Set q -> [Sym] -> Set q
-- final ts ss [] = ss
-- final ts ss (a : as) = final ts (transition ts ss a) as
-- final ts = foldl (transition ts)
 
accepts :: (Ord q) => FSM q -> [Sym] -> Bool 
accepts (FSM _ _ ts ss fs)  string = (not.null) (fs /\ final)
  where final = foldl (transition ts) ss string

-- Q 8.
trace :: Ord q => FSM q -> [Sym] -> [Set q]
trace (FSM qs as ts ss fs) word = reverse (tr ss word)  where 
  tr ss [] = [ss]
  tr ss (w : ws) = reverse(init(transition ts ss w : tr ss ws)) ++ tr ss []

-- Q 9.
reverseFSM :: Ord q => FSM q -> FSM q
reverseFSM (FSM qs as ts ss fs) =
  FSM qs as undefined fs ss

-- Example machines

m1 :: FSM Int
m1 = mkFSM
     [0,1,2,3,4] -- states
     "ab"        -- symbols
     [ (0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2), (1,'b',4)
     , (2,'a',3), (2,'b',3), (3,'b',4), (4,'a',4), (4,'b',4) ]
     [0]  -- starting
     [4]  -- accepting

m2 :: FSM Char
m2 = mkFSM
     "ABCD"      -- states
     "01"        -- symbols
     [('A', '0', 'D'), ('A', '1', 'B'), ('B', '0', 'A'), ('B', '1', 'C'),
      ('C', '0', 'B'), ('C', '1', 'D'), ('D', '0', 'D'), ('D', '1', 'D')]
     "B"   -- starting
     "ABC" -- accepting

dm1 :: FSM [Int] 
dm1 = mkFSM 
      [[],[0],[1,2],[3],[3,4],[4]] -- states
      "ab"                         -- symbols
      [([],   'a',[]),    ([],   'b',[])
      ,([0],  'a',[1,2]), ([0],  'b',[1,2])
      ,([1,2],'a',[3]),   ([1,2],'b',[3,4])
      ,([3],  'a',[]),    ([3],  'b',[4])
      ,([3,4],'a',[4]),   ([3,4],'b',[4])
      ,([4],  'a',[4]),   ([4],  'b',[4])]
      [[0]]       -- starting
      [[3,4],[4]] -- accepting


-- Q 10.
extraction :: (Ord q) => [(Set q)] -> (Set q)
extraction xs = fromList(concat[toList x | x <- xs])

ddelta :: (Ord q) => FSM q -> (Set q) -> Char -> (Set q)
ddelta (FSM  qs as ts ss fs ) source sym = extraction[transition ts (fromList [x]) sym | x <- toList source]

-- Q 11.

next :: (Ord q) => FSM q -> Set(Set(q)) -> Set(Set(q))
next fsm@(FSM qs as ts ss fs) supers =
   fromList [ ddelta (FSM  qs as ts ss fs ) super sym | super <- toList supers, sym <- toList as ]

-- Q 12.
-- reachable :: (Ord q) => FSM q ->  Set(Set(q)) -> Set(Set(q))
-- reachable fsm@(FSM qs as ts ss fs) supers =
--   let new = undefined
--   in if null new then supers else reachable fsm (supers \/ new)

-- Q 13.
dfinal :: (Ord q) => FSM q -> Set(Set(q)) -> Set(Set(q))
dfinal  fsm@(FSM qs as ts ss fs) supers = undefined

--Q 14.
dtrans :: (Ord q) => FSM q -> Set(Set q) -> Set(Trans (Set q))
dtrans fsm@(FSM qs as ts ss fs) supers =
  fromList [ (q, s, undefined) | q <- toList supers, s <- toList as ]

-- Q 15.
toDFA :: (Ord q) => FSM q -> FSM (Set q)
toDFA fsm@(FSM qs as ts ss fs) = FSM qs' as' ts' ss' fs'
  where
  qs' = undefined
  as' = as
  ts' = undefined
  ss' = undefined
  fs' = undefined
