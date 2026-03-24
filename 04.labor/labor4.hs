-- # 4. labor

-- I. Definiáljuk azt a Haskell-listát, amely tartalmazza:

-- - az első n páros szám négyzetét,

negyzet n = take n [ i ^ 2 | i <- [2, 4 ..]]

nagyzet2 n = take n $ map (\i -> i ^ 2) [2, 4 ..]

negyzet3 n = mapM_ (\(szam, negyzete) -> putStrLn (show szam++ "negyzete " ++ show negyzete)) ls
    where
        ls = take n $ map (\i -> (i, i ^ 2)) [2, 4 ..]


-- - az első $$[1, 2, 2, 3, 3, 3, 4, 4, 4, 4,\ldots]$$,

szamoklS 1 = replicate 1 1
szamokLs n = szamokLs (n-1) ++ replicate n n

szamokLs2 n i
    |i /= n = replicate i i ++ szamokLs2 n  (i+1)
    |otherwise = replicate i i


-- - az első $$[2, 4, 4, 6, 6, 6, 8, 8, 8, 8\ldots]$$,

szamokLs3 n i j 
    | i/= n = replicate i (j+2) ++ szamokLs3 n (i+1) (j+2)
    |otherwise = replicate i (j+2)

szamokLs4 n i
    |i /= n = replicate i (i*2) ++ szamokLs4 n  (i+1)
    |otherwise = replicate i (i*2)

-- - az első $$[n, n-1, \ldots, 2, 1, 1, 2, \ldots, n-1, n]$$,

szamokLs5 n = [n, n-1 .. 1] ++ [1 .. n]

-- - váltakozva tartalmazzon True és False értékeket,

valtakozo n = take n ls
    where
        ls = [True,False] ++ ls


-- - váltakozva tartalmazza a $$0,\ 1,\ -1$$ értékeket.

valtakozo2 n = take n ls
    where
        ls = [0,1,-1] ++ ls

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy adott szám osztóinak számát,

osztok x = Length [i | i<- [1 .. x], mod x i == 0]

osztok2 x = mylength [i | i<- [1 .. x], mod x i == 0]
    where
        mylength [] = 0
        mylength (_ : ls) = 1 + mylength ls

osztok3 x = fold1 (\res i -> if mod x i == 0 then res + 1 else res) 0 [1 .. x]

osztok4 x = fold1 (\res i -> if mod x i == 0 then res + 1 else res) 1 [1 .. div x 2]

-- - meghatározza egy adott szám legnagyobb páratlan osztóját,

maxparatlan n = last [i | i <- [1, 3 .. n], mod n i == 0]

maxparatlan2 n = maximum [i | i <- [1, 3 .. n], mod n i == 0, odd i]

maxparatlan3 n = maximum [i | i <- [1, 3 .. n], mod n i == 0]

maxparatlan4 n = mymaxim [i | i <- [1, 3 .. n], mod n i == 0]
    where
            mymaxim [x] = x
            mymaxim (x1:x2:xs)
                |x1>x2 = mymaxim (x1:xs)
                |otherwise = mymaxim (x2:xs)

maxparatlan5 n 
    | odd n = n
    |otherwise = foldl1 (\acc x -> if mod n x == 0 then x else acc) 1 [1,3 .. n]


-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,

decP x p 
    |x<p = [x]
    |otherwise = decP (div x p) p ++ [mod x p]

decPszam x p = length $ decP x p
-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjegy,

decPmax x p = maximum $ decP x p
-- - meghatározza az $a$ és $b$ közötti Fibonacci számokat, $a > 50$.

fibo a b = dropwhile (<a) $ fibosg 0 1 0
    where
            fibosg a1 b1 res
                |res <= b = fibosg b1 res (res+b1)
                |otherwise = [res]

fibo2 = fibosg 0 1 0 
    where

-- III. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista pozitív elemeinek átlagát,
atlag ls = (sum ls) / fromIntegral (length ls)

atlagp ls = atlag [x | x <- ls,x>0]

atlagp2 ls = atlag . filter (>0) $ ls

atlag3 ls =(sum ls1) / fromIntegral (length ls1)
    where 
        ls1 = filter(>0 ls)

-- - meghatározzuk azt a listát, amely tartalmazza az eredeti lista minden n-ik elemét,

listan ls n = [i | (idx, i)<- zip[1..] ls, mod i n == 0]

listan2 ls n i
    | i>= length ls = []
    | mod i n==0 = ls !! i:listaN2 ls n (i+1)
    |otherwise = listan2 ls n (i + 1)


listan3 ls n = map snd $ filter (\x -> mod(fst x) n == 0) (zip [1..] ls)

-- - tükrözi egy lista elemeit,

tukroz ls = reverse ls

tukroz2 ls = map (\x -> read x :: INT ) $ map (reverse . show) ls

digitstonumber = foldl (\acc d -> acc *10 +d) 0

tukor3 ls = map (digitstonumber . tukorszam) ls
    where
        tukorszam x
            |x<10 = [x]
            |otherwise = mod x 10 : tukorszam (div x 10)


-- - két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy hogy a lista elemeit csak egyszer járja be,
maxelempoz ls = [idx | (idx, i) <- zip [1 ..] ls, i == mymax]
    where
        mymax = maximum ls

maxelempoz2 (x:ls) = foldl aux(x, [0]) (zip ls [1..])
    where
        aux (currentmax, currentmaxi) (elem, i)
            |elem > currentmaxi = (elem,[i])
            |elem== curentmax = (elem, positions ++ [i])
            |otherwise = (currentmax, positions)

-- - meghatározza egy lista leggyakrabban előforduló elemét.

elof ls =map (\x -> (x,length)) $ (group . sort) ls

elof2 ls = maxelofe
    where
        elofszam = maximum $ map length $ (group . sort) ls
        ls2 = map (\x -> (x,length x)) $ (group . sort) ls
        maxelofe = head $ fst $ head $ filter (\x -> snd x == elofszam) ls2



main = do
    negyzet
    print (negyzet2 4)