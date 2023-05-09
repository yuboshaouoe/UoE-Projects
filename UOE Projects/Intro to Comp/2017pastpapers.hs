-- 1.a

f :: [Int] -> Int
f [] = 0
f xs = sum([x^2 | x <- xs, x `mod` 3 == 0, x `mod` 5 /= 0])

-- 1.b

g :: [Int] -> Int
g [] = 0
g (x:xs)
    | x `mod` 3 == 0 && x `mod` 5 /= 0 = x^2 + g xs
    | otherwise = g xs

-- 1.c

prop_fg :: [Int] -> Bool
prop_fg xs = f xs == g xs

-- 2.a

mst :: Int -> Int -> Bool
mst x y
    | x >= 0 && y > 2*x = True
    | x <= 0 && y > x `div` 2 = True
    | otherwise = False

-- 2.b

ordered :: [Int] -> Bool
ordered xs
    | null([xs!!k | k <- [0..length xs], xs!!k >= xs!!(k+1)]) = True
    | otherwise = False

-- 2.c

ordered' :: [Int] -> Bool
ordered' (x:[]) = True
ordered' (x':x:xs)
    | x' <= x = ordered' (x:xs)
    | otherwise = False
