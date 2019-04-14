addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product[1..n]

factorial' :: Integer -> Integer
factorial' 0 = 1
factorial' n = n * factorial (n - 1)

circumference :: Float -> Float
circumference r = 2 * pi * r

--Int is bounded to the to either 32/64 depending on your hardware
--Integer is not bounded
--Float is a single floating point
--Double is a double floating point
--Num is a Type class that consists of Int, Integer, Float and double
--Floating is a Type class that consists of Float and Double
--Integral is a Type class that consists of Int and Integer


-- Probably not the best way to declare it but trying to follow along
lengthAndAdd :: (Num a1) => [a2] -> a1 -> a1
lengthAndAdd l a = fromIntegral(length l) + a
