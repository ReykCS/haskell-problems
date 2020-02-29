--- FOR CHECKING CHECKSUM ---
-- input: bitstream with checksum -> generatorpolynom, output: bool if bitstream is flawless
checkCRCchecksum :: [Bool] -> [Bool] -> Bool
checkCRCchecksum xs as
                    | (==) (length testChecksum) 0 = True
                    | otherwise = False
                    where testChecksum = calcChecksum xs as

--- FOR CHECKSUM CALCULATION ---
-- input: bitstream -> generatorpolynom, output: bitstream + crcChecksum
createCRCchecksumArray :: [Bool] -> [Bool] -> [Bool]
createCRCchecksumArray [] _ = [];
createCRCchecksumArray xs [] = xs
createCRCchecksumArray xs polynom = addChecksumToArray xs (calcChecksum xs polynom)

addChecksumToArray :: [Bool] -> [Bool] -> [Bool]
addChecksumToArray stream checksum = xorArray stream (pad checksum (length stream))

calcChecksum :: [Bool] -> [Bool] -> [Bool]
calcChecksum [] _ = []
calcChecksum xs as
                | (<) (length xs) (length as) = xs
                | otherwise = calcChecksum (cut (xorArray xs as)) as


--- ASSETS ---
xorArray :: [Bool] -> [Bool] -> [Bool]
xorArray [] _ = []
xorArray xs [] = xs
xorArray (x:xs) (a:as) = [(xor) x a] ++ (xorArray xs as)

cut :: [Bool] -> [Bool]
cut [] = []
cut (False:xs) = cut xs
cut xs = xs

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

pad :: [Bool] -> Int -> [Bool]
pad xs 0 = xs
pad xs num
        | (<) (length xs) num = [False] ++ (pad xs (num - 1))
        | otherwise = xs

--- TESTCASE ---
test :: [Bool]
test = createCRCchecksumArray [True, False, False, True, False, True, False, True, False, True, True, False, False, False, False, False] [True, True, False, False, True, True]

testResolve :: Bool
testResolve = checkCRCchecksum test [True, True, False, False, True, True]

-- CRC array -> [True, False, False, True, False, True, False, True, False, True, True, False, False, False, False, False]
-- generatorpolynom [True, True, False, False, True, True]