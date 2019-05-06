-- Chapter 5 Folds
-- A fold takes a binary function, an accumulator and a list to fold up

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- which is the same as
-- magic (or as the functions are curried we will return a function that takes a list)
sum'' :: (Num a) => [a] -> a
sum'' = foldl + 0


