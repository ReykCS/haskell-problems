sequ :: [String]
sequ = [m | let lst = ['a'..'c'], i <- lst, j <- lst, k <- lst, 
                        i /= j, j /= k, i /= k, let m = i : j : k : []]

multiply :: Int -> [Int]
multiply n = multiply' [n, (n-1)..1] [1..n]

multiply' :: [Int] -> [Int] -> [Int]
multiply' (l:[]) (a:[]) = [a*l]
multiply' (l:lst) (a:acc) = [(l*a)] ++ (multiply' lst acc)


multList :: [Int] -> [Int]
multList (x:xs) = l (x:xs) x
                where 
                    l (x:[]) acc = [x*acc]
                    l (x:xs) acc = [x*acc] ++ (l xs (acc * x))

