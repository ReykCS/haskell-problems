data Complex = Complex {re :: Double, im :: Double} deriving Show

dft :: [Double] -> [Complex]
dft xs = let len = getLength xs in map (\x -> calcDft xs x 0 len (Complex 0 0)) [0.0..(len - 1)]

getLength :: [Double] -> Double
getLength xs = read (show (length xs)) :: Double

calcDft :: [Double] -> Double -> Double -> Double -> Complex -> Complex
calcDft [] _ _ _ c = c
calcDft (x:xs) n k len (Complex r i) = calcDft xs n (k+1) len (Complex (r + newR) (i + newI))
                            where 
                                e = - ((2.0 * pi * k * n) / len) 
                                newR = x * (cos e)
                                newI = x * (sin e)