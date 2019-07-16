import qualified Data.Map as Map
import Data.Char

phonebook = 
    [("betty", "111-5552")
    ,("james", "444-1112")
    ,("james", "111-8272")
    ,("chloe", "123-4567")
    ,("chloe", "099-1233")
    ]

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

strings2Digits :: String -> [Int]
strings2Digits = map digitToInt . filter isDigit

phonebookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phonebookToMap xs = Map.fromListWith add xs
    where add number1 number2 = number1 ++ ", " ++ number2

phonebookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phonebookToMap' xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs

-- blows my mind a bit
-- Map.fromListWith max [(2,2), (4,10), (10,300), (4,5), (2,100), (10,2)]