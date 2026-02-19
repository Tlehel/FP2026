osszeg a b = a + b

kulonbseg a b = a - b

szorzat a b = a * b

divide a b = a / b

osztmar a b = mod a b

osztmar2 a b = a 'mod' b

elsof a b = (-b) / a

abszolut a 
    | a<0 = -a
    | otherwise = a

abszolut2 a = if a < 0 then -a else a

elojel n = if n < 0 then "neg" else if n > 0 then "poz" else "nulla"

elojel2 n 
    | n < 0 = "neg"
    | n > 0 = "poz"
    | otherwise = "nulla"

max a b = if a > b then a else b

max1 a b
    | a > b = a
    | otherwise = b

min a b = if a < b then a else b

min1 a b
    | a < b = a
    | otherwise = b

