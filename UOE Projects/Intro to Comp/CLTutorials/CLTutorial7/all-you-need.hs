-- | The following Haskell coding is of the universe of people from Chapter 16 of the textbook.

data Person = Angela | Ben | Claudia | Diana | Emilia 
            | Fangkai | Gavin | Hao | Iain deriving (Eq,Show)

people :: [Person]
people = [Angela, Ben, Claudia, Diana, Emilia, 
          Fangkai, Gavin, Hao, Iain]
          
type Relation u = u -> u -> Bool

loves :: Relation Person
Angela `loves` Ben = True
Angela `loves` _ = False

Ben `loves` Gavin = True
Ben `loves` Claudia = True
Ben `loves` _ = False

Claudia `loves` Angela = True
Claudia `loves` Ben = True
Claudia `loves` Emilia = True
Claudia `loves` _ = False

Diana `loves` Hao = True
Diana `loves` Iain = True
Diana `loves` _ = False

Emilia `loves` Fangkai = True
Emilia `loves` _ = False 

Fangkai `loves` Diana = True
Fangkai `loves` _ = False

Gavin `loves` Fangkai = True
Gavin `loves` _ = False

Hao `loves` Fangkai = True
Hao `loves` Diana = True
Hao `loves` _ = False

Iain `loves` Emilia = True
Iain `loves` _ = False

-- | You may find the following predicates useful.

type Predicate u = u -> Bool

neg :: Predicate u -> Predicate u
(neg p) x = not (p x)

(|:|) :: Predicate u -> Predicate u -> Predicate u
(p |:| q) x = p x || q x

(&:&) :: Predicate u -> Predicate u -> Predicate u
(p &:& q) x = p x && q x

(-:>) :: Predicate u -> Predicate u -> Predicate u
p -:> q = neg p |:| q

every :: [u] -> Predicate u -> Bool
every xs p = and [ p x | x <- xs ]

some :: [u] -> Predicate u -> Bool
some xs p = or [ p x | x <- xs ]

-- [ Your Haskell contribution begins here ]

-- | Exercise 3.1

somebodyLovesSomeoneWhoLovesThem :: Bool
somebodyLovesSomeoneWhoLovesThem = some people (\x -> some people (\y -> x `loves` y && y `loves` x))

-- | Exercise 3.2

everybodyLovesSomeoneWhoLovesSomeoneWhoLovesThem :: Bool
everybodyLovesSomeoneWhoLovesSomeoneWhoLovesThem = every people (\x -> some people (\y -> some people (\z -> x `loves` y && y `loves` z && z `loves` x)))

-- | Exercise 4
existsUnique :: Eq u => [u] -> Predicate u -> Bool
existsUnique xs p = some xs (\x -> every y (\y -> p x && not (p y) && x /= y))
    where y = [m | m <- xs]
-- why don't we use this? existsUnique xs p = length [x | x <- xs, p x] == 1