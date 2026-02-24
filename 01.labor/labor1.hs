osszeg a b = a + b

kulonbseg a b = a - b

szorzat a b = a * b

divide a b = a / b

osztmar a b = mod a b

osztmar2 a b = a `mod` b

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

negyzetgyok n = [sqrt i | i <- [1..n]]

negyzetN :: (Enum a, Floating a) => a ->[a]
negyzetN n = [ i*i | i <- [1..n]]

kobN n = [i^3 | i <- [1..n]]

--nemnegyzetN :: Int -> [int]
--nemnegyzetN n = [i | i <- [1..n], (sqrt i * sqrt * )/= i]

xhatvany x n = [  x^ i | i <- [1..n]]

osztokN n = [i | i <- [1 .. n] , n `mod` i == 0,mod i 2 ==0 ]

osztokN2 n = [i | i <- [2,4 .. n] , mod n i ==0 ]

osztok n = [i | i <- [1.. n] , mod n i ==0 ]

primszam n = osztok n ==[1,n]

primszamokN n = [i | i<- [2..n], primszam i ]

primszamokN2 n = [i | i<- [2..n], primszamL i ]
    where
        primszamL n = osztokL == [1,n]
        osztokL = [i | i <-[1..n],mod n i == 0]

osszetettN n = [i| i <- [0..n], not (primszam i)]

paratlanoszetettN n = [i |i<-[0..n],not (primszam i),mod i 2 /=0]

paratlanoszetettN2 n = [i |i<-[3,5..n],not (primszam i)]

betuszam = zip ['a' .. 'z'][0..25]

szamok1 = zip [0..5][5,4..0]

szamok2 n = [(i,n - i)|i <- [0..n]]

tfls n = take n ls 
    where
        ls = [True, False]++ ls



main::IO()
main = do
    putStrLn " x hatvany n"
    print (xhatvany 5 3)
    putStrLn " osztok"
    print (osztokN 48)
    print (paratlanoszetettN 100)