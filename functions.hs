-- Chapter 3 syntax in functions

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER 7!!"
lucky x = "Outta luck!"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- same as fst
first :: (a, b, c) -> a
first (x, _, _) = x

-- same as snd
second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "You cant call head on an empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list has loads of elements! The first two are: " ++ show x ++ " and " ++ show y

-- As Pattern
firstLetter :: String -> String
firstLetter "" = "Empty string!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guard
bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, eat more"
    | bmi <= 25.0 = "Bang on"
    | bmi <= 30.0 = "You're overweight, let's work out!"
    | otherwise = "You're obese. Go see a doctor"

max' :: (Ord a) => a-> a -> a
max' a b
    | a <= b    = b
    | otherwise = a

bmiTell' :: Double ->  Double -> String
bmiTell' weight height 
    | bmi <= skinny     = "You're underweight, eat more"
    | bmi <= normal     = "Bang on"
    | bmi <= overweight = "You're overweight, let's work out!"
    | otherwise         = "You're obese. Go see a doctor"
    where bmi        = weight / height ^ 2
          (skinny, normal, overweight) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- need to have another look at this. 
calBmis :: [(Double, Double)] -> [Double]
calBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

cylider :: Double -> Double -> Double
cylider r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

describeList :: [a] -> String
describeList ls = "The list is " ++ what ls
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."


