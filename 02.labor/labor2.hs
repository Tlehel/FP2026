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

szjszamls ls = map szjszam ls

szjOsszeg n
    |n<0 = szjOsszeg (abs n)
    |div n 10 == 0 = mod n 10
    |otherwise= mod n 10 + szjOsszeg (div n 10)

szjosszeg2 n res 
    |n<0 = szjosszeg2 (abs n) res
    |n<10= res+n
    |otherwise = szjosszeg2 (div n 10) (res + mod n 10)

szjosszegls ls = map szjosszeg ls

szjosszegls2 ls = map (\x -> (x, szjosszeg x)) ls

paros n
    |n<0 = paros (abs n)
    |n<10 = if even n then 1 else 0
    |otherwise = if even (mod n 10) then 1 + paros (div n 10) else paros (div n 10)

lgszj n lg
    |n<0 = lgszj (abs n) lg
    |n<10 = if lg>n then lg else n
    |otherwise = if mod n 10 > lg then lgszj (div n 10) (mod n 10) else lgszj (div n 10) lg

bszam n b d
    |n<0 = error "neg"
    |n<b = if n==d then 1 else 0
    |otherwise = if mod n b==d then 1 +bszam(div n b) b d else bszam (div n b) b d

fibon n  = fibo 0 1 0 n
    where
        fibo__res 0 = res
        fibo a b res n1 = fibo b res (b+res) (n1-1)

fibosg 0 1 0 n
    |fibosg__res 0 = res
    |otherwise = fibosg a b res n1 = fibosg b res (b+res) (n1-1)

fibosz n = map (fisg 0 1 0) [o..n]

ls2 = [(577723707, 7),(0,1),(2847,9)]

szamoszegls2 ls = map (uncurry szamjosszeg2) ls2

parosls ls = map paros ls

main :: IO ()
main = do
    let fel1 = szjszorzat 1234
    print fel1