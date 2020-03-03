import System.Random
import Control.Monad (replicateM)

problem1 :: [a] -> a
problem1 (x:[]) = x
problem1 (x:xs) = problem1 xs

problem2 :: [a] -> a
problem2 (x:[]) = x
problem2 (x:y:[]) = x
problem2 (x:xs) = problem2 xs

problem3 :: [a] -> Int -> a
problem3 lst index = l lst index 1
                    where
                        l (x:lst) index i
                            | (==) index i = x
                            | otherwise = l lst index (i+1)

problem4 :: [a] -> Int
problem4 lst = count lst 0
            where
                count [] acc = acc
                count (x:xs) acc = count xs (acc+1)

problem5 :: [a] -> [a]
problem5 [] = []
problem5 (x:xs) = (problem5 xs) ++ [x]

problem6 :: Eq a => [a] -> Bool
problem6 [] = False
problem6 (x:[]) = True
problem6 lst = palindrome lst 0 (length lst)
            where
                palindrome (l:xs) i j
                                | j < i = True
                                | ((l:xs)!!(j-(1-i))) == ((l:xs)!!(i)) = palindrome xs (i+1) (j-1)
                                | otherwise = False 

problem7 :: [[a]] -> [a]
problem7 = foldr (++) [] 

problem8 :: String -> String
problem8 [] = []
problem8 (x:y:[]) = [y]
problem8 (x:y:lst)
                | (==) x y = problem8 (y:lst)
                | otherwise = [x] ++ problem8 (y:lst)

problem9 :: String -> [String]
problem9 [] = []
problem9 lst = [prob] ++ (problem9 (drop (length prob) lst))
                where prob = problem9' lst (head lst)

problem9' :: String -> Char -> String
problem9' [] _ = []
problem9' (x:str) ctr
                    | (==) x ctr = [x] ++ (problem9' str ctr) 
                    | otherwise = []

problem10 :: String -> [(Int, Char)]
problem10 lst = problem10' (problem9 lst)

problem10' :: [String] -> [(Int, Char)]
problem10' [] = []
problem10' (x:str) = [((length x), (head x))] ++ (problem10' (str))

data ListElement = Multiple Int Char | Single Char deriving Show

problem11 :: String -> [ListElement]
problem11 str = problem11' (problem10 str)

problem11' :: [(Int, Char)] -> [ListElement]
problem11' [] = []
problem11' ((1, a):xs) = [Single a] ++ (problem11' xs)
problem11' ((a, b):xs) = [Multiple a b] ++ (problem11' xs)

problem12 :: [ListElement] -> String
problem12 [] = []
problem12 ((Single a):xs) = [a] ++ (problem12 xs)
problem12 ((Multiple a b):xs) = (problem12' a b) ++ (problem12 xs)

problem12' :: Int -> Char -> String
problem12' 0 _ = []
problem12' num clr = [clr] ++ (problem12' (num - 1) clr)

problem13 :: String -> [ListElement]
problem13 [] = []
problem13 (x:xs)
                | (==) count 1 = [(Single x)] ++ (problem13 (drop (count-1) xs))
                | otherwise = [(Multiple count x)] ++ (problem13 (drop (count - 1) xs))
                where count = problem13' (x:xs) x

problem13' :: String -> Char -> Int
problem13' [] _ = 0
problem13' (x:xs) ctr
                    | (==) x ctr = 1 + problem13' xs ctr
                    | otherwise = 0

-- duplicate every list elements
problem14 :: [a] -> [a]
problem14 [] = []
problem14 (x:xs) = x : x : (problem14 xs)

-- repeat element a given number
problem15 :: [a] -> Int -> [a]
problem15 [] _ = []
problem15 (x:xs) num = (problem15' x num) ++ (problem15 xs num)
    
problem15' :: a -> Int -> [a]
problem15' chr 0 = []
problem15' chr num = [chr] ++ (problem15' chr (num - 1))

-- Drop every N'th element from a list.
problem16 :: [a] -> Int -> [a]
problem16 [] _ = []
problem16 str num = problem16' str num (num - 1)

problem16' :: [a] -> Int -> Int -> [a]
problem16' [] _ _ = []
problem16' (x:xs) num act
                        | (==) act 0 = (problem16' xs num (num - 1))
                        | otherwise = x : (problem16' xs num (act - 1))

-- Split a list into two parts; the length of the first part is given.
problem17 :: [a] -> Int -> [[a]]
problem17 [] _ = []
problem17 xs num = [take num xs, drop num xs]

-- Extract a slice from a list.
problem18 :: [a] -> Int -> Int -> [a]
problem18 [] _ _ = []
problem18 str start end = drop (start - 1) (take end str)

-- Rotate a list N places to the left.
problem19 :: [a] -> Int -> [a]
problem19 str 0 = str
problem19 lst num
                    | (>=) num 0 = (drop num lst) ++ (take num lst)
                    | otherwise = (drop ((length lst) - (-num)) lst) ++ (take ((length lst) + num) lst)

-- Remove the K'th element from a list. -- Not edge case safe
problem20 :: [a] -> Int -> (a, [a])
problem20 str num = (getIndex str num, problem20' str num)
                where
                    getIndex (x:str) 1 = x
                    getIndex (x:str) num = getIndex str (num - 1)

problem20' :: [a] -> Int -> [a]
problem20' [] _ = []
problem20' (x:str) 1 = str
problem20' (x:str) num = [x] ++ (problem20' str (num - 1))

-- Insert in list at position
problem21 :: a -> [a] -> Int -> [a]
problem21 _ [] _ = []
problem21 chr lst 1 = [chr] ++ lst
problem21 chr (x:lst) num = [x] ++ (problem21 chr lst (num-1))

problem22 :: Int -> Int -> [Int]
problem22 a b = [a..b]

-- problem23 :: Int -> IO [Int]
-- problem23 num = replicateM 10 (randomIO :: IO Int)

-- convertToInt :: IO [Int] -> [Int]
-- convertToInt [] = []
-- convertToInt (x:xs) = [x] ++ (convertToInt xs)

-- getRandomNumbers :: Int -> IO [Int]
-- getRandomNumbers num = replicateM 10 (randomIO :: IO Int)

problem26 :: [String]
problem26 = [a | let m = ['a'..'l'], i <- m, j <- m, k <- m, i /= j, j /= k, k /= i,
                let a = i : j : k : []]

-- isPrime ??
problem31 :: Int -> Bool
problem31 num = (==) (length [a | a <- [2..(num - 1)], (mod num a) == 0]) 0 

-- greatest divisor
problem32 :: Int -> Int -> Int
problem32 a b = (problem32' (problem32'' (findDivisors a)) (problem32'' (findDivisors b)))

problem32'' :: [Int] -> [Int]
problem32'' [] = []
problem32'' (x:xs) = (problem32'' xs) ++ [x]

problem32' :: [Int] -> [Int] -> Int
problem32' (x:xs) (l:ls)
                        | (>) x l = problem32' xs (l:ls)
                        | (<) x l = problem32' (x:xs) ls
                        | otherwise = x

findDivisors :: Int -> [Int]
findDivisors a = [b | b <- [1..a], mod a b == 0]

-- is coPrime ??
problem33 :: Int -> Int -> Bool
problem33 a b
            | (==) (problem32 a b) 1 = True
            | otherwise = False 

problem34 :: Int -> Int
problem34 num = length [a | a <- [1..(num-1)], problem33 num a]

--- determine Prime factors
problem35 :: Int -> [Int]
problem35 num = problem35'' [x | x <- [2..(num-1)], let lst = [m | m <- [2..(x-1)], (mod x m) == 0], length lst == 0] num

problem35'' :: [Int] -> Int -> [Int]
problem35'' [] _ = []
problem35'' (x:xs) num = (problem35' num x x) ++ (problem35'' xs num)

problem35' :: Int -> Int -> Int -> [Int]
problem35' num prime mult
                        | (>=) mult num = []
                        | (==) (mod num mult) 0 = [prime] ++ (problem35' num prime (mult * prime))
                        | otherwise = []

problem36 :: Int -> [(Int, Int)]
problem36 num = problem36' (problem35 num)

problem36' :: [Int] -> [(Int, Int)]
problem36' [] = []
problem36' (x:xs) = let amount = problem36'' xs x 1 in [(x, amount)] ++ (problem36' (drop (amount-1) xs))

problem36'' :: [Int] -> Int -> Int -> Int
problem36'' [] num res = res
problem36'' (x:xs) num res
                    | (==) x num = problem36'' xs num (res+1)
                    | otherwise = problem36'' xs num res

-- eulers totient
problem37 :: Int -> Int
problem37 num = foldr (\x y -> (*) (calc x) y) 1 (problem36 num)
            where 
                calc x = pow (((fst x) - 1) * (fst x)) ((snd x) - 1)

pow :: Int -> Int -> Int
pow num 0 = 1
pow num 1 = num
pow num n = num * (pow num (n-1))

-- Prime numbers in range
problem39 :: Int -> Int -> [Int]
problem39 a b = [c | c <- [a..b], let list = [d | d <- [2..(c - 1)] , mod c d == 0], length list == 0]

-- goldbach problem
problem40 :: Int -> (Int, Int)
problem40 num = head (filter (\x -> checkPrime x) (map (\x -> (x, num - x)) [2..(num - 1)]))
                where
                    checkPrime x = (&&) (isPrime (fst x)) (isPrime (snd x))

isPrime :: Int -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime num = length [a | a <- [2..(num-1)], mod num a == 0] == 0

-- goldbach list
problem41 :: Int -> Int -> [(Int, Int)]
problem41 a b = map (problem40) [c | c <- [a..b], mod c 2 == 0]

problem41_2 :: Int -> Int -> [(Int, Int)]
problem41_2 a b = filter (\x -> (>=) (fst x) 50) (problem41 a b)

problem41_3 :: Int -> Int -> Int 
problem41_3 a b = length (problem41_2 a b)

