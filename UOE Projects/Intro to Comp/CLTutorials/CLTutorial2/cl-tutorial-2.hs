-- Exercise 1
-- Every one is the ood one out since they each hold an unique property.

----------------------------------------------------------------
data Thing = A | B | C | D | E deriving (Eq, Show)
things :: [Thing]
things = [A, B, C, D ,E]

-- Exercise 2

data Colour = Yellow | Blue deriving (Eq, Show)
data Shape = Circle | Square deriving (Eq, Show)
data Size = Big | Small deriving (Eq, Show)
data Edge = Bold | Normal deriving (Eq, Show)

colour :: Thing -> Colour
colour A = Yellow
colour B = Yellow
colour C = Yellow
colour D = Blue
colour E = Yellow

shape :: Thing -> Shape
shape A = Square
shape B = Square
shape C = Circle
shape D = Square
shape E = Square

size :: Thing -> Size
size A = Big
size B = Big
size C = Big
size D = Big
size E = Small

edge :: Thing -> Edge
edge A = Bold
edge B = Normal
edge C = Bold
edge D = Bold
edge E = Bold

-- Exercise 3

checkIfBlue :: Thing -> Bool
checkIfBlue x = x `elem` [n | n <- things, colour n == Blue]

checkIfYellow :: Thing -> Bool
checkIfYellow x = x `elem` [n | n <- things, colour n == Yellow]

-- and so on.

-- Exercise 4

type Predicate a = a -> Bool
isCircle, isSquare, isBlue, isYellow, isBold, isNormal, isBig, isSmall :: Predicate Thing

isSmall E = True
isSmall _ = False
isBig x = not (isSmall x)

isCircle C = True
isCircle _ = False
isSquare x = not (isCircle x)

isBlue D = True
isBlue _ = False
isYellow x = not (isBlue x)

isNormal B = True
isNormal _ = False
isBold x = not (isNormal x)

thingsOtherThan :: Thing -> [Thing]
thingsOtherThan x = [n | n <- things, n /= x]

properties :: [Predicate Thing]
properties = [isBlue, isYellow, isBig, isSmall, isNormal, isBold, isSquare, isCircle]

propertiesOf :: Thing -> [Predicate Thing]
propertiesOf x = [ xs | xs <- properties, xs x == True]

isPropertyOfAnotherThing :: Predicate Thing -> Thing -> Bool
isPropertyOfAnotherThing p t
    | p t == False = error "input Thing doesn't have input Predicate"
    | [] == [ x | x <- things, p x, x /= t] = False
    | otherwise                             = True

propertiesOnlyOf :: Thing -> [Predicate Thing]
propertiesOnlyOf x = [p | p <- propertiesOf x, isPropertyOfAnotherThing p x == False]

rank :: Thing -> Int
rank x = length ([propertiesOnlyOf x])

-- Exercise 5

-- 1. and [isNormal x | x <- things, isBlue x && isSquare x]
-- 2. and [isSquare x | x <- things, not (isYellow x)]
-- 3. and [isBold x || isYellow x | x <-things, isBig x && isSquare x]
-- 4. and [isBig x | x <- things, not (isYellow x && isCircle x)]

-- Exercise 6
-- and [isBlue x | x <- things, not (isSquare x)]
-- or [isBlue x | x <- things, not(isSquare x)]

-- Exercise 7 (During Tutorial session)
