module Main where


-- Custom sum function
customSum :: Num a => [a] -> a
customSum [] = 0
customSum (x:xs) = x + customSum xs

customLength :: [a] -> Int
customLength [] = 0
customLength (_:xs) = 1 + customLength xs

-- Custom abs function
customAbs :: (Ord a, Num a) => a -> a
customAbs x = if x < 0 then -x else x

-- Custom foldr function
customFoldr :: (a -> b -> b) -> b -> [a] -> b
customFoldr _ z []     = z
customFoldr f z (x:xs) = f x (customFoldr f z xs)


promptNumber :: String -> IO Int
promptNumber str = do
    putStrLn str
    input <- getLine
    return (read input :: Int)

countDividers :: Int -> Int

convertDividerValue :: (Integral a1, Num a2) => a1 -> a1 -> a2
convertDividerValue n x = if n `div` x == x then 1 else 2

countDividers n = customSum [convertDividerValue n x | x <- [1..n], n `mod` x == 0 && x * x <= n]

findMinDividers :: Int -> Int -> Int
findMinDividers x n = if
    countDividers x >= n
    then x
    else findMinDividers (x + 1) n

solve7 :: IO Int
solve7 = do
    n <- promptNumber "Podaj liczbe do zadania7: "
    return (findMinDividers 1 n)

(~==) :: (Ord a, Fractional a) => a -> a -> Bool
(~==) x y = customAbs (x - y) < 0.000001
(~/=) :: (Ord a, Fractional a) => a -> a -> Bool
(~/=) x y = not (x ~== y)

doesTriangleExist :: (Ord a, Num a) => a -> a -> a -> Bool
doesTriangleExist a b c = a + b < c || a + c < b || b + c < a

isFittingTriangle :: (Ord f, Floating f) => f -> f -> f -> f -> f -> f -> f -> Bool
isFittingTriangle r bx by cx cy ax ay = customAbs bx + customAbs by <= r &&
    ((bx-ax)*(cy-ay)) ~/= ((by-ay)*(cx-ax)) && doesTriangleExist ((bx - cx) ** 2  + (by - cy) ** 2)
            (bx ** 2 + by ** 2)
            ( cx ** 2 + cy ** 2)

sumFittingTriangles :: Double -> Int
sumFittingTriangles r = customLength [(x, y) | x <- [-r..r], y <- [-r..r],
    isFittingTriangle r x y ( r / 4) (r / 4) 0 0]

solve40 :: IO Int
solve40 = do
    r <- promptNumber "Zadanie40 r: "
    return (sumFittingTriangles $ fromIntegral r)

generatePrimes :: Int -> [Int]
generatePrimes n = sieve [2..n]
    where
        sieve [] = []
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0] -- filter out all multiples of a prime number


isDividerNotOk :: Integral a => a -> a -> a -> Bool
isDividerNotOk x b p = x `mod` p == 0 && p > b

hasPrimeDividersLessThan :: Integral t => [t] -> t -> t -> Bool
hasPrimeDividersLessThan ps x b
  = foldr (\ p -> (&&) (not (x `mod` p == 0 && p > b))) True ps

solve38 :: IO Int
solve38 = do
    n <- promptNumber "Zadanie38 n:"
    b <- promptNumber "Zadanie38 b:"
    let primes = generatePrimes n
    return (length [x | x <- [1..n], hasPrimeDividersLessThan primes x b])


main :: IO ()
main = do
    result7 <- solve7
    print result7
    result38 <- solve38
    print result38
    result40 <- solve40
    print result40