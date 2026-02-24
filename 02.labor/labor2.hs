szjszorzat 0 = 1
szjszorzat x 
    | x<0 = error "neg szam"
    |otherwise = mod x 10 * szjszorzat (div x 10)

szjszorzat2 x
    |x<0 = error "neg szam"
    |x == 0 = 1
    |otherwise = mod x 10 * szjszorzat2 (div x 10)
szjosszeg 0 = 0
szjosszeg x = mod x 10 + szjosszeg ( div x 10)

szjosszeg2 x 
    |x < 0 = error "negszam"
    |x == 0 = 0
    |otherwise = mod x 10 + szjosszeg2 ( div x 10)
szjszam 0 = 0
szjszam x =  1 + szjszam (div x 10)

szjszam2 x
    |x < 0 = szjszam2 (abs x)
    |x==0 =0
    |otherwise = 1 + szjszam2 (div x 10)



main :: IO ()
main = do
    let fel1 = szjszorzat 1234
    print fel1