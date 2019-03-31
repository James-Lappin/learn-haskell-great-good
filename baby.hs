doubleMe x = x + x

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <-xs, odd x]

length' xs = sum [1 | _ <- xs]

removeUpperCase st = [c | c <- st, c `elem` ['A'..'Z']]

