-- Indexed data represented as a tree

module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT,
                    prop_set_get, prop_toList_fromList
                  )

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right)
    | size left >= size right = 1 + depth left
    | size left <  size right = 1 + depth right

-- Exercise 7

quicksort :: Ord k => [(k,a)] -> [(k,a)]
quicksort []         = []
quicksort ((k,a):xs) = quicksort lesser ++ [(k,a)] ++ quicksort greater
    where
        lesser  = [(p,q) | (p,q) <- xs, p < k]
        greater = [(p,q) | (p,q) <- xs, p >= k]

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node k a left right) = quicksort([(k,a)] ++ toList left ++ toList right)

-- Exercise 8

-- b) when go encounters a leaf, it sets and returns a Node which has
--    the given key and value.
-- c) when go encounter the key it was looking for, it sets the value
--    of that Node with the corresponding key to the given value.

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = go
    where go Leaf = Node key value Leaf Leaf
          go (Node k v left right) | key == k = Node k value left right
                                   | key < k  = Node k v (go left) right
                                   | key > k  = Node k v left (go right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get key = find
    where find Leaf = Nothing
          find (Node k v left right) | key == k = Just v
                                     | key < k  = find left
                                     | key > k  = find right

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList = foldr (uncurry set) Leaf


prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 12

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT = undefined

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT = undefined

-- Exercise 13

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge = undefined

-- Exercise 14

del :: Ord k => k -> Keymap k a -> Keymap k a
del = undefined

-- Exercise 15

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select = undefined 

-- Instances for QuickCheck -----------------------------
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary
