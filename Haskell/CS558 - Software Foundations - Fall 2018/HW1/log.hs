ghci> :l funs.hs 
[1 of 1] Compiling Main             ( funs.hs, interpreted )
Ok, modules loaded: Main.


-- 1.1 (a) --
isEven :: Int -> Bool
isEven n = n`mod`2==0

allEven :: [Int] -> Bool
allEven [] = True
allEven (x:xs) = isEven x && allEven xs

--test
ghci> allEven []
True
ghci> allEven [1,2,3]
False
ghci> allEven [8,2,6]
True


-- 1.1 (b) --
fp :: Int -> [Int] -> [Int]
fp i [] = []
fp i (x:xs) = x*i : (fp (i+1) xs)

f x = fp 0 x

--test
ghci> f []
[]
ghci> f [1,2,3,4,5]
[0,2,6,12,20]
ghci> f [2,3,7,9]
[0,3,14,27]


--1.2 (a) --
quadsolns :: Float -> Float -> Float -> [Float]
quadsolns a b c
    | delta<0 = []
    | delta>=0 = [(-b-sqrt(delta))/(2*a),(-b+sqrt(delta))/(2*a)]
    where delta = b^2-4*a*c

-- 1.2 (b) --
ghci> quadsolns 2 (-5) 3
[1.5,1.0]
ghci> quadsolns 1 4 4
[-2.0,-2.0]
ghci> quadsolns 2 5 4
[]


-- 1.3 (a) --
isDivisible :: Int -> Int -> Bool
isDivisible x y = fromIntegral (x`div`y) == fromIntegral x / fromIntegral y

existDivisible :: Int -> [Int] -> Bool
existDivisible x [] = False
existDivisible x (y:ys) = isDivisible x y || existDivisible x ys

isPrime :: Int -> Bool
isPrime x
    | x<2 = False
    | x==2 = True
    | otherwise = not (existDivisible x [2..n])
    where n = (floor (sqrt (fromIntegral x))) + 1

--test
ghci> isPrime (-1)
False
ghci> isPrime 0
False
ghci> isPrime 1
False
ghci> isPrime 2
True
ghci> isPrime 3
True
ghci> isPrime 4
False
ghci> isPrime 5
True
ghci> isPrime 6
False
ghci> isPrime 7
True


-- 1.3 (b) --
ghci> filter isPrime [1..30]
[2,3,5,7,11,13,17,19,23,29]


-- 1.3 (c) --
ghci> let allPrimes = filter isPrime [1..]
ghci> take 5 allPrimes
[2,3,5,7,11]
ghci> take 20 allPrimes
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]
ghci> take 100 allPrimes
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,
113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,
239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,
373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,
503,509,521,523,541]


-- 1.3 (d) --
{-
In "let allPrimes = filter isPrime [1..]", I only define how to calculate the list allPrimes. Because of the laziness feature, Haskell won't evaluate the infinite list allPrimes until it has to. Thus, we can define an infinite list without looping forever.
-}
