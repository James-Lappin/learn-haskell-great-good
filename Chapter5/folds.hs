-- Chapter 5 Folds
-- A fold takes a binary function, an accumulator and a list to fold up

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- which is the same as
-- magic (or as the functions are curried we will return a function that takes a list)
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

-- Rigth fold
mapr :: (a -> b) -> [a] -> [b]
mapr f xs = foldr (\x acc -> f x : acc) [] xs

-- 
mapl :: (a -> b) -> [a] -> [b]
mapl f xs = foldl (\acc x -> acc ++ [f x]) [] xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

reverse' :: [a] -> [a]
reverse' = foldl(\acc x -> x : acc) []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x:acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- function application
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x

sqrtSums' :: Int
sqrtSums' = length (takeWhile (<1000) (scanl1 (+) $ map sqrt [1..])) + 1 

funcApplic :: [Double]
funcApplic = map ($ 3) [(4+), (10*), (^2), sqrt]

-- function composition
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)
negateLambda :: [Int]
negateLambda = map (\x -> negate (abs x)) [5, -3, -6, 7, -3, 2, -19, 24]

negateComp :: [Int]
negateComp = map (negate . abs) [5, -3, -6, 7, -3, 2, -19, 24]
