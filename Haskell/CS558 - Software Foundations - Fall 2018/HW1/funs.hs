-- 1.1 (a) --
isEven :: Int -> Bool
isEven n = n`mod`2==0

allEven :: [Int] -> Bool
allEven [] = True
allEven (x:xs) = isEven x && allEven xs

-- 1.1 (b) --
fp :: Int -> [Int] -> [Int]
fp i [] = []
fp i (x:xs) = x*i : (fp (i+1) xs)

f x = fp 0 x

--1.2 (a) --
quadsolns :: Float -> Float -> Float -> [Float]
quadsolns a b c
    | delta<0 = []
    | delta>=0 = [(-b-sqrt(delta))/(2*a),(-b+sqrt(delta))/(2*a)]
    where delta = b^2-4*a*c

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
