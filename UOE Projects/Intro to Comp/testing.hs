angleVectors :: Float -> Float -> Float -> Float -> Float
angleVectors a b a' b' = acos phi
    where phi = dotProduct a b a' b' / (vectorLength a b * vectorLength a' b')
          dotProduct a b a' b'= a * a' + b * b'
          vectorLength a b = sqrt (a ^ 2 + b ^ 2)

root :: Float -> Float -> Float -> Float
root a b c = (-b + sqrt(b ^ 2 - 4 * a * c)) / 2 * a

hour :: Int -> Int
hour n = (1 + n `div` 60) `mod` 12

between :: Int -> Int -> Int -> Int
between a b c 
        |(a <= b && b <= c)||(c <= b && b <= a)  = b
        |(b <= c && c <= a)||(a <= c && c <= b)  = c
        |(c <= a && a <= b)||(b <= a && a <= c)  = a

xor :: Bool -> Bool -> Bool
xor a b 
    | a==True && b==True = False
    | a==True && b==False || a==False && b==True = True
    | otherwise = False

metre_to_feet :: Float -> (Int, Int)
metre_to_feet metres = (feets, inches)
    where feets = floor(metres * 3.28084)
          inches = round (metres * 39.37008)

nameAge :: (String,Int) -> String
nameAge (s,n) = s ++ "(" ++ show n ++ ")"

sqrtsecond :: [Float] -> Float
sqrtsecond (_:a:_) = sqrt a
sqrtsecond [] = -1.0
sqrtsecond (_:[]) = -1.0

isPrime :: Int -> Bool
isPrime n = null [ n | x <- [2..n-1], n `mod` x == 0 ]

angleVectors2 :: [(Float, Float)] -> Float
angleVectors2 [(a,b), (a',b')] = acos phi
    where phi = dotProduct [(a,b), (a',b')] / (vectorLength (a,b) * vectorLength (a',b'))
          dotProduct [(a,b), (a',b')] = a * a' + b * b'
          vectorLength (a,b) = sqrt (a ^ 2 + b ^ 2)

halveEvens :: [Int] -> [Int]
halveEvens ns = [n `div` 2 | n <- ns, n `rem` 2 == 0]

type Line = (Float, Float)
intersection :: Line -> Line -> (Float, Float)
intersection (a, b) (a', b') = ((b' - b) / (a - a'), a * (b' - b) / (a - a') + b)

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b ns = [n | n <- ns, a <= n && n <= b]

oddsRec :: [Int] -> [Int]
oddsRec [] = []
oddsRec (x:xs) 
    | odd x = x : oddsRec xs
    | otherwise = oddsRec xs

countFromToBy :: Int -> Int -> Int -> [Int]
countFromToBy from to by
    | from > to = []
    | from <= to = from : countFromToBy (from+by) to by
	
h = foldr (*) 1 . map cube . filter neg
    where
        cube x  =  x * x * x
        neg x   =  x < 0

double :: [Int] -> [Int]
double xs = [2*x | x <- xs]

fromDecimal :: Int -> [Int]
fromDecimal 0 = [0]
fromDecimal n = mod n 2 : fromDecimal (div n 2)

toBinary :: Int -> [Int]
toBinary n = reverse(fromDecimal n)

grey :: Int -> [[Int]]
grey n = undefined

partitions :: Int -> [[Int]]
partitions 0 = [[]]
partitions n | n > 0 = [ k : xs | k <- [1..n],
                                  xs <- partitions (n-k),
                                  all (k <=) xs ]