{-# LANGUAGE FlexibleInstances #-}

-- | The following code is needed in order to do QuickCheck tests
-- involving predicates. We do not expect you to understand this,
-- although you should already be familiar with things and predicates.

import Test.QuickCheck
    ( Arbitrary, arbitrary, elements
    , CoArbitrary,coarbitrary,variant
    , quickCheck )

data Thing = A | B | C | D | E deriving (Eq,Show)

things :: [Thing]
things = [ A, B, C, D, E ]

instance Arbitrary Thing where
    arbitrary = elements [A, B, C, D, E]

instance CoArbitrary Thing where
    coarbitrary A = variant 0
    coarbitrary B = variant 1
    coarbitrary C = variant 2
    coarbitrary D = variant 3
    coarbitrary E = variant 4

type Predicate u = u -> Bool
                    
instance Show (u -> Bool) where
    show p = "a predicate"

--------------------------------------------------------------------------------

-- | The next three operations correspond to the negation,
-- disjunction, and conjunction of predicates, respectively. You
-- should already be familiar with them from previous tutorials.

neg :: Predicate u -> Predicate u
(neg p) x = not (p x)

(|:|) :: Predicate u -> Predicate u -> Predicate u
(p |:| q) x = p x || q x

(&:&) :: Predicate u -> Predicate u -> Predicate u
(p &:& q) x = p x && q x

--------------------------------------------------------------------------------

-- | The next two relations provide support for sequents.
-- We have discussed both of them in Tutorial 3.

-- | (|=) corresponds to sequents with one antecedent and
-- one succedent.

(|=) :: Predicate Thing -> Predicate Thing -> Bool
p |= q = and [ q x | x <- things, p x ]

-- | (||=) corresponds to sequents with several (a list of)
-- antecedents and one succedent.
        
(||=) :: [Predicate Thing] -> Predicate Thing -> Bool
ps ||= q = and [ q x | x <- things, and [p x | p <- ps] ]

-- [ Now your work begins... ]
          
(|||=) :: [Predicate Thing] -> [Predicate Thing] -> Bool
gamma |||= delta = null [q | q <- delta, gamma ||= q == False]

prop1 :: Predicate Thing -> Predicate Thing -> Bool
prop1 p q = p |= q == [p] |||= [q]

prop2 :: [Predicate Thing] -> Predicate Thing -> Bool
prop2 ps q = ps ||= q == ps |||= [q]