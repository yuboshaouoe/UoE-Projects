-- Informatics 1 - Introduction to Computation
-- Computation and Logic Tutorial 9
--
-- Week 10 (16-22 Nov.)
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, our solutions
-- require only a few of lines for each answer. If your code is getting complicated
-- you're making things too difficult for yourself---try to keep it simple!
module Tutorial10 where
import Prelude hiding (lookup)
import Data.Set(Set, insert, empty, member, fromList, toList,
                 union, intersection, size, singleton, (\\),
                 cartesianProduct, disjointUnion)
import qualified Data.Set as Set
import Test.QuickCheck
import Data.Char


------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------ NFA -- machines with epsilon-transitions-------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

------------------------------------------------------------------------------------ 
------  NFA states symbols transitions e-transitions starting accepting ------------
------------------------------------------------------------------------------------ 
data NFA q = NFA(Set q)(Set Sym)(Set(Trans q))(Set (q,q))(Set q)(Set q)deriving Show
mkNFA :: Ord q => [q] -> [Sym] -> [(q, Sym, q)] -> [(q, q)] -> [q] -> [q] -> NFA q
mkNFA qs as ts es ss fs =  -- a convenience function constructing NFA from lists
  NFA (fromList qs)(fromList as)(fromList ts)(fromList es)(fromList ss)(fromList fs)

---------------------- conversion from FSM to NFA ----------------------------------
asNFA :: FSM q -> NFA q
asNFA (FSM qs as ts ss fs) = NFA qs as ts empty ss fs
-- converting an NFA with no e-transitions to an FSM
asFSM :: NFA q -> FSM q
asFSM  (NFA qs as ts es ss fs)
  | null es   = FSM qs as ts ss fs
  | otherwise = error "has e-transitions"

eStep  :: Ord q =>  Set (q,q) -> Set q -> Set q
eStep es qq = fromList [ q' | (q, q') <- toList es, q `member` qq ]

-- Q1.
eClose :: Ord q => Set(q,q) -> Set q -> Set q   
eClose es qq = 
  let new = eStep es qq \\ qq
  in if null new then qq else eClose es (qq \/ new)

ddeltaNFA :: (Ord q) => NFA q -> (Set q) -> Sym -> (Set q)
ddeltaNFA (NFA  qs as ts es ss fs ) source  sym =
  eClose es (transition ts source sym)

acceptsNFA :: (Ord q) => NFA q -> [Sym] -> Bool 
acceptsNFA nfa@(NFA _ _ ts es ss fs)  string = (not.null) (fs /\ final)
  where final = foldl (ddeltaNFA nfa) (eClose es ss) string
  
nextNFA :: (Ord q) => NFA q -> Set(Set q) -> Set(Set q)
nextNFA nfa@(NFA qs as ts es ss fs) supers =
   fromList [ ddeltaNFA nfa super sym | super <- toList supers, sym <- toList as ]

reachableNFA :: (Ord q) => NFA q ->  Set(Set q) -> Set(Set q)
reachableNFA nfa supers =
  let new = nextNFA nfa supers \\ supers
  in if null new then supers else reachableNFA nfa (supers \/ new)

dfinalNFA :: (Ord q) => NFA q -> Set(Set q) -> Set(Set q)
dfinalNFA  nfa@(NFA qs as ts es ss fs) supers = filterS (any (`member` fs)) supers

dtransNFA :: (Ord q) => NFA q -> Set(Set q) -> Set(Trans (Set q))
dtransNFA nfa@(NFA qs as ts es ss fs) supers =
  fromList [ (q, s, ddeltaNFA nfa q s) | q <- toList supers, s <- toList as ]

-- Q2. no coding required ...
fromNFAtoDFA :: (Ord q) => NFA q -> NFA (Set q)
fromNFAtoDFA nfa@(NFA qs as ts es ss fs) = NFA qs' as' ts' empty ss' fs'
  where
  qs' = reachableNFA nfa ss'
  as' = as
  ts' = dtransNFA nfa qs'
  ss' = singleton (eClose es ss)
  fs' = dfinalNFA nfa qs'

{-The ss' term is different as it needs to find all possible actived states
  as there are multiple start states.-}
----------------------------------- Basic regex ------------------------------------

-- the machines constructed below may assume this property for any NFA arguments
-- under this assumption they must guarantee this property for the NFA they return
isThompson :: Eq a => NFA a -> Bool
isThompson (NFA qs as ts es ss fs) = case (toList ss, toList fs) of
  ([s],[f]) -> (not . or) [ q'==s||q==f | (q,_,q') <- toList ts] &&
               (not . or) [ q'==s||q==f | (q,q') <- toList es]
  _ -> False

-- Q3a.
stringNFA :: String -> NFA Int
stringNFA xs = mkNFA qs as ts es ss fs where
  qs = [0..n]
  as = xs
  ts = [(a,xs!!a,a+1) | a <- [0..n-1]]
  es = []
  ss = [0]
  fs = [n]
  n = length xs

-- Q3b.
{-
nullNFA :: NFA Bool -- NB ensure this is Thompson
nullNFA = mkNFA qs as ts es ss fs where
  qs = [True]
  as = []
  ts = []
  es = []
  ss = [True]
  fs = [] 
-}

nullNFA :: NFA Bool -- NB ensure this is Thompson
nullNFA = mkNFA qs as ts es ss fs where
  qs = [False,True]
  as = []
  ts = []
  es = [(False,True)]
  ss = [False]
  fs = [True]

-- Q3c.
dotNFA :: NFA Bool
dotNFA = mkNFA qs as ts es ss fs where
    qs = [False,True]
    as = ['a'..'z']
    ts = [(False,c,True) | c <- as]
    es = []
    ss = [False]
    fs = [True]

------------------------------- Constructing Machines ------------------------------

data QF q = Q | E q | F deriving (Eq, Ord,Show)

--a map function for transitions
mapTrans :: (Ord a, Ord b) => (a -> b) -> Set(Trans a) -> Set(Trans b)
mapTrans f = mapS (\(q, a, q') -> (f q, a, f q'))

-- a map function for e-transitions
mapEs :: (Ord a, Ord b) => (a -> b) -> Set (a, a) -> Set (b, b)
mapEs f = mapS (\ (q, q')-> (f q, f q'))

-- Q4. no coding required
thompson :: Ord q => NFA q -> NFA (QF q)
thompson (NFA qs as ts es ss fs) =
  NFA qs' as ts' es' ss' fs' where
   qs' = ss' \/ fs' \/ mapS E qs
   ts' = mapTrans E ts
   es' = mapEs E es 
         \/
         fromList [ (Q, E q) | q <- toList ss ]
         \/
         fromList [(E q, F) | q <- toList fs ]
   ss' = singleton Q
   fs' = singleton F

-- Q5. no coding required
concatNFA :: (Ord a, Ord b) => NFA a -> NFA b -> NFA (Either a b)
concatNFA (NFA qs as ts es ss fs)(NFA qs' as' ts' es' ss' fs') =
  NFA qs'' as'' ts'' es'' ss'' fs'' where
    qs'' = mapS Left qs \/ mapS Right qs' -- = disjointUnion qs qs'
    as'' = as \/ as'
    ts'' = mapTrans Left ts \/ mapTrans Right ts'
    es'' = mapEs Left es \/ mapEs Right es'
           \/
           cartesianProduct (mapS Left fs) (mapS Right ss')
    ss'' = mapS Left  ss
    fs'' = mapS Right fs'

-- Q6.
unionNFA :: (Ord q, Ord q') => NFA q -> NFA q' -> NFA (Either q q')
unionNFA (NFA qs as ts es ss fs) (NFA qs' as' ts' es' ss' fs') =
  NFA qs'' as'' ts'' es'' ss'' fs'' where
  qs'' = disjointUnion qs qs'
  as'' = as \/ as'
  ts'' = mapTrans Left ts \/ mapTrans Right ts'
  es'' = mapEs Left es \/ mapEs Right es'
  ss'' = disjointUnion ss ss'
  fs'' = disjointUnion fs fs'

-- Q7.
tUnionNFA :: (Ord q, Ord q') => NFA q -> NFA q' -> NFA (QF (Either q q'))
tUnionNFA m n = thompson(unionNFA m n)

-- Q8.
tStarNFA :: Ord q => NFA q -> NFA (QF q)
tStarNFA (NFA qs as ts es ss fs) =
  NFA qs' as ts' es' ss' fs'
  where qs' = ss'\/fs'\/ mapS E qs
        ts' = mapTrans E ts
        es' = fromList [ (Q, F), (Q, E (the ss)), (E (the fs), F)
                       , (E (the fs), E (the ss)) ]
              \/
              mapEs E es
        ss' = singleton Q
        fs' = singleton F
        the xs = case toList xs of
                   [x] -> x
                   _   -> error "no unique the in tStarNFA"


--------------------------------------- Regex --------------------------------------

data Regex =
  S String | Regex0 | Dot
  | (:|:) Regex Regex -- alternation
  | (:>:) Regex Regex -- concatenation
  | Star Regex        -- Kleene-star
  deriving Show

-- Q9.
regex2nfa :: Regex -> NFA Int
regex2nfa (S s)     = stringNFA s
regex2nfa Regex0    = intNFA nullNFA
regex2nfa Dot       = intNFA dotNFA
regex2nfa (r :|: s) = intNFA $ tUnionNFA (regex2nfa r) (regex2nfa s)
regex2nfa (r :>: s) = intNFA $ concatNFA (regex2nfa r) (regex2nfa s)
regex2nfa (Star r)  = intNFA $ tStarNFA (regex2nfa r)

-- Q10abc
-- (tidyFSM . minimalDFA . regex2nfa) ((Star $ S "a") :|: (Star $ S "b"))
{- FSM (fromList [0,1,2]) (fromList "ab") (fromList [(0,'a',0),(1,'a',0),(1,'b',2),(2,'b',2)]) 
       (fromList [1]) (fromList [0,1,2])
-}

-- (tidyFSM . minimalDFA . regex2nfa) (Star (S "a" :|: Star (S "a" :>: S "b")))
{- FSM (fromList [0,1]) (fromList "ab") (fromList [(0,'a',1),(1,'a',1),(1,'b',0)]) 
       (fromList [0]) (fromList [0,1])
-}

-- (tidyFSM . minimalDFA . regex2nfa) Regex0
{- FSM (fromList [0]) (fromList "") (fromList []) (fromList [0]) (fromList [0])
-}

-- Q11.
sentence = "how many states are there in the minimal dfa that recognises the"
  ++" language consisting of the words in this sentence all in lower case"
-- (minimalDFA. regex2nfa) (foldr (:|:) Regex0 $ map S $ words sentence)
-- There are 62 total states, ss = fromList [1], fs = fromList[4,5]
-- as = fromList "acdefghilmnorstuwy".

-- Q12.
-- 

---------------- You do not need to change any code below this line ----------------

intNFA :: (Ord q) => NFA q -> NFA Int
intNFA (NFA qs as ts es ss fs)  = 
  NFA qs' as ts' es' ss' fs' where
  qs' = mapS qToInt qs
  ts' = mapTrans qToInt ts
  es' = mapEs qToInt es 
  ss' = mapS qToInt ss
  fs' = mapS qToInt fs
  qToInt = lookup (zip (toList qs) [0..])
  lookup :: (Ord q) => [(q,Int)] -> q -> Int
  lookup qis q' = the [ i | (q,i) <- qis, q == q' ]
  the [q] = q
  the _ = error "lookup"

reverseNFA :: Ord q => NFA q -> NFA q
reverseNFA (NFA qs as ts es ss fs) =
  NFA qs as ts' es' ss' fs' where
  ts' = mapS (\ (q,a,q') -> (q',a,q)) ts  
  es' = mapS (\ (q,q')   -> (q',q)) es
  ss' = fs
  fs' = ss

fromFSMtoDFA :: (Ord q) => FSM q -> FSM (Set q)
fromFSMtoDFA fsm@(FSM qs as ts ss fs) = FSM qs' as' ts' ss' fs'
  where
  qs' = reachableFSM fsm ss'
  as' = as
  ts' = dtransFSM fsm qs'
  ss' = singleton ss
  fs' = dfinalFSM fsm qs'

minimalDFA :: Ord q => NFA q -> FSM Int
minimalDFA =  asFSM.intNFA.fromNFAtoDFA.reverseNFA.fromNFAtoDFA.reverseNFA

pruneFSM :: Ord q => FSM q -> FSM q
pruneFSM (FSM qs as ts ss fs) = FSM qs' as ts' ss (fs/\qs')
  where reach qq =
          let  new = fromList[ q' | (q,_,q') <- toList ts, q`member` qq]\\qq
          in if null new then qq else reach (qq\/ new)
        qs' = reach ss
        ts' = fromList [ t | t@(q,_,_) <- toList ts, q`member`qs']

tidyFSM :: Ord q => FSM q -> FSM Int
tidyFSM = intFSM . reverseFSM . pruneFSM . reverseFSM . pruneFSM


-- infix operations on sets
(\/) :: Ord a => Set a -> Set a -> Set a
(\/) = Set.union
(/\) :: Ord a => Set a -> Set a -> Set a
(/\) = Set.intersection

-- map and filter for sets
mapS :: Ord b => (a -> b) -> Set a -> Set b
mapS = Set.map 
filterS :: Ord a => (a -> Bool) ->  Set a -> Set a
filterS = Set.filter

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------ FSM code from tutorial 9 ----------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

type Sym = Char
type Trans q = (q, Sym, q)

-- FSM states symbols transitions starting accepting ---------------
data FSM q = FSM (Set q) (Set Sym) (Set(Trans q)) (Set q) (Set q) deriving Show
mkFSM :: Ord q => [q] -> [Sym] -> [Trans q] -> [q] -> [q] -> FSM q 
mkFSM qs as ts ss fs =  -- a convenience function constructing FSM from lists
  FSM (fromList qs) (fromList as) (fromList ts) (fromList ss) (fromList fs)

isDFA :: Ord q => FSM q -> Bool
isDFA (FSM qs as ts ss fs) = (size ss == 1)
  && and[ length[ q' | q' <- toList qs, (q, a, q')`member`ts ] == 1
        | q <- toList qs, a <- toList as ]

transition :: (Ord q) => Set(Trans q)  -> Set q -> Sym -> Set q
transition ts qs s  = fromList [ q' | (q, t, q') <- toList ts, t == s, q `member` qs ]

acceptsFSM :: (Ord q) => FSM q -> [Sym] -> Bool 
acceptsFSM (FSM _ _ ts ss fs)  string = (not.null) (fs /\ final)
  where final = foldl (transition ts) ss string

traceFSM :: Ord q => FSM q -> [Sym] -> [Set q]
traceFSM (FSM qs as ts ss fs) word = tr ss word where 
  tr ss' [] = [ ss' ]
  tr ss' (w : ws) = ss' : tr (transition ts ss' w) ws 

reverseFSM :: Ord q => FSM q -> FSM q
reverseFSM (FSM qs as ts ss fs) =
  FSM qs as (mapS (\(q,a,q') ->  (q',a,q)) ts) fs ss

ddeltaFSM :: (Ord q) => FSM q -> (Set q) -> Char -> (Set q)
ddeltaFSM (FSM qs as ts ss fs ) source  sym = transition ts source sym

nextFSM :: (Ord q) => FSM q -> Set(Set q) -> Set(Set q)
nextFSM fsm@(FSM qs as ts ss fs ) supers =
   fromList [ ddeltaFSM fsm super sym | super <- toList supers, sym <- toList as ]

reachableFSM :: (Ord q) => FSM q ->  Set(Set q) -> Set(Set q)
reachableFSM fsm@(FSM qs as ts ss fs) supers =
  let new = nextFSM fsm supers \\ supers
  in if null new then supers else reachableFSM fsm (supers \/ new)

dfinalFSM :: (Ord q) => FSM q -> Set(Set q) -> Set(Set q)
dfinalFSM  fsm@(FSM qs as ts ss fs) supers = filterS (any (`member` fs)) supers

dtransFSM :: (Ord q) => FSM q -> Set(Set q) -> Set(Trans (Set q))
dtransFSM fsm@(FSM qs as ts ss fs) supers =
  fromList [ (q, s, ddeltaFSM fsm q s) | q <- toList supers, s <- toList as ]

intFSM :: (Ord q) => FSM q -> FSM Int
intFSM (FSM qs as ts ss fs)  = 
  FSM 
  (mapS qToInt qs)
  as
  (mapTrans qToInt ts)
  (mapS qToInt ss)
  (mapS qToInt fs)
  where
    qToInt = lookup (zip (toList qs) [0..])
    lookup :: (Ord q) => [(q,Int)] -> q -> Int
    lookup qis q' = the [ i | (q,i) <- qis, q == q' ]
    the [q] = q
    the _ = error "lookup"
    
---- Miscellaneous examples

simple :: FSM Int
simple = mkFSM [0..20] "abcd" [(0,'a',1),(1,'c',2),(1,'d',2)][0][2]

m1 :: FSM Int
m1 = mkFSM
     [0,1,2,3,4] -- states
     "ab"    -- alphabet
     [ (0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2), (1,'b',4)
     , (2,'a',3), (2,'b',3), (3,'b',4), (4,'a',4), (4,'b',4) ]
     [0]  -- start
     [4]  -- accept

m2 :: FSM Char
m2 = mkFSM
     "ABCD"          -- states
     "01"        -- symbols
     [('A', '0', 'D'), ('A', '1', 'B'), ('B', '0', 'A'), ('B', '1', 'C'),
      ('C', '0', 'B'), ('C', '1', 'D'), ('D', '0', 'D'), ('D', '1', 'D')]
     "B"   -- start
     "ABC" -- accept

dm1 :: FSM [Int] 
dm1 = mkFSM 
      [[],[0],[1,2],[3],[3,4],[4]] -- states
      "ab"                     -- symbols
      [([],   'a',[]),    ([],   'b',[])
      ,([0],  'a',[1,2]), ([0],  'b',[1,2])
      ,([1,2],'a',[3]),   ([1,2],'b',[3,4])
      ,([3],  'a',[]),    ([3],  'b',[4])
      ,([3,4],'a',[4]),   ([3,4],'b',[4])
      ,([4],  'a',[4]),   ([4],  'b',[4])]
      [[0]]       -- start
      [[3,4],[4]] -- accept

m3 :: NFA Int
m3 =  mkNFA [0..5] "ab" [ (1,'a',2), (3,'b',4)] [ (0,1), (2,3), (0,5), (4,5), (4,1)] [0] [5]

-- -- toy examples
g0 :: [Int] -> [Int] -> FSM Int
g0 = mkFSM [0..9] "37" [ (n,intToDigit m, m*n `mod` 10) | n <- [0..9], m <- [3,7] ]
eg0 = g0 [1]   [9]
eg1 = g0 [1,2] [9]



