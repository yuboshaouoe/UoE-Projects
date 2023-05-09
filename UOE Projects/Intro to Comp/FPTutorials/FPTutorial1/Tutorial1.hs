module Tutorial1 where

import PicturesSVG -- needed for the optional chess part
import Test.QuickCheck

-- Exercise 2:

double :: Int -> Int
double x = x + x

square :: Int -> Int
square x = x ^ 2

-- Exercise 3:

isTriple :: Int -> Int -> Int -> Bool
isTriple a b c
        | a^2+b^2 == c^2 = True
        | a^2+c^2 == b^2 = True
        | b^2+c^2 == a^2 = True
        | otherwise      = False

-- Exercise 4:

leg1 :: Int -> Int -> Int
leg1 x y = x^2-y^2

leg2 :: Int -> Int -> Int
leg2 x y = 2*x*y

hyp :: Int -> Int -> Int
hyp x y = x^2+y^2

-- Exercise 5:

prop_triple :: Int -> Int -> Bool
prop_triple x y = isTriple (leg1 x y) (leg2 x y) (hyp x y)

-- Exercise 7:

pic1 :: Picture
pic1 = fourPictures knight

pic2 :: Picture
pic2 = twoBeside knight `above` flipV(twoBeside knight)

-- Functions --

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)

-- Exercise 8:

twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = (twoAbove (twoBeside x))

-- Exercise 9:
-- a)

whiteblack :: Picture
whiteblack = (whiteSquare `beside` blackSquare)

emptyRow :: Picture
emptyRow = repeatH 4 whiteblack

-- b)

otherEmptyRow :: Picture
otherEmptyRow = flipV (emptyRow)

-- c)

twoRow :: Picture
twoRow = emptyRow `above` otherEmptyRow

middleBoard :: Picture
middleBoard = twoRow `above` twoRow

-- d)

whitePiece :: Picture
whitePiece = rook `beside` knight `beside` bishop `beside` queen `beside` king `beside` bishop `beside` knight `beside` rook

whiteRow :: Picture
whiteRow = whitePiece `over` otherEmptyRow

blackRow :: Picture
blackRow = invert (whitePiece) `over` emptyRow

-- e)

whitePawn :: Picture
whitePawn = pawn `beside` pawn `beside` pawn `beside` pawn `beside` pawn `beside` pawn `beside` pawn `beside` pawn

whitePawnRow :: Picture
whitePawnRow = whitePawn `over` emptyRow

white :: Picture
white = whitePawnRow `above` whiteRow

blackPawnRow :: Picture
blackPawnRow = invert (whitePawn) `over` otherEmptyRow

black :: Picture
black = (blackRow `above` blackPawnRow)

populatedBoard :: Picture
populatedBoard = black `above` middleBoard `above` white
