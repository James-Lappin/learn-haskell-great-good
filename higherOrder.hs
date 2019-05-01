-- Chapter 5 higher order functions 

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f(f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x
-- is the same as
flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f x y = f y x

-- usage 
-- zipWith' div [2,2..] [10, 8, 6 , 4, 2]
-- zipWith' flip' (div) [2,2..] [10, 8, 6 , 4, 2]

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let smallerOrEqual = filter (<= x) xs
        larger         = filter (> x) xs
    in  quickSort smallerOrEqual ++ [x] ++ quickSort larger

-- as haskell is lazy it doesnt matter if the list is infinite
-- evalution will stop when the first adequate solution is found
largestDivisible :: Integer
largestDivisible = head (filter p [99999, 99998..])
    where p x = x `mod` 3829 == 0


-- filter vs list comprehensions
-- filter takes for ever
sumOddSquaresWithFilter :: Integer
sumOddSquaresWithFilter = sum (takeWhile (<10000) (filter odd (map (2^) [1..])))

-- comp is quick
sumOddSquaresWithComp :: Integer
sumOddSquaresWithComp = sum (takeWhile (<10000) [m | m <- [n^2 | n <-[1..]], odd m])

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd  n = n : chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

numLongChainsLambda :: Int
numLongChainsLambda = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15