problem1 :: Int
problem1 = foldr (+) 0 [mult | mult <- [1..999], (||) (mod mult 3 == 0) (mod mult 5 == 0)]

problem2 :: Int -> Int -> Int
problem2 start pref
                | (>=) ((+) start pref) 4000000 = 0
                | even ((+) start pref) = (+) ((+) start pref) (problem2 pref ((+) start pref))
                | otherwise = (problem2 pref ((+) start pref))

-- Not finished
problem3 = [m | m <- [600851475143,600851475142..1], let c = [j | j <- [(div (m-1) 2), ((div (m-1) 2) - 1)..2], (mod m j) == 0], length c == 0]

isPalindrome :: Int -> Bool
isPalindrome num = (==) (read (rev (show num)) :: Int) num

rev :: String -> String
rev [] = []
rev (x:[]) = [x]
rev (x:xs) = (rev xs) ++ [x]

problem4 :: Int
problem4 = foldr (\x y -> if x >= y then x else y) 0 [m*g | m <- [100..999], g <- [100..999], isPalindrome (g*m)]


-- problem5 :: Integer -> Integer
-- problem5 start = let sum = (filterDivs (foldr (+) 0 [1..start])) in
--             if ((>=) (length sum) 500) then
--                 start
--             else problem5 (start+1)
problem12 :: Int -> Int -> Int
problem12 act sum
            | (>=) (filterDivs sum) 500 = sum
            | otherwise = problem12
             (act + 1) $! (sum + act)

filterDivs :: Int -> Int
filterDivs divs = length (filter (\x -> (==) (mod divs x) 0) [1..divs])

problem5 :: [Int]
problem5 = [a | a <- [1..], (foldr (&&) True (map (\x -> mod a x == 0) [1..20]))]

problem6 :: Int -> Int 
problem6 num = (-) (squareSum num) (sumSquares num)

sumSquares :: Int -> Int
sumSquares end = foldr (+) 0 [a * a| a <- [1..end]]

squareSum :: Int -> Int
squareSum num = a * a
            where
                a = (foldr (+) 0 [1..num])

problem10 :: Int -> Int
problem10 num = foldr (+) 0 [x | x <- [2..num], let c = [i | i <- [2..(x-1)], (mod x i) == 0], length c == 0]

-- Coin Sums
problem31 :: Int -> [Int]
problem31 = [x | x <- [1, 2, 5, 10, 20, 50, 100, 200]]

