import System.Win32 (xBUTTON1)
-- # 3. labor

-- I. Mit csinálnak az alábbi függvényhívások, ahol az atlag a számok átlagát meghatározó függvény?

-- ```haskell
atlag :: (Floating a) => [a] -> a
atlag ls = (sum ls) / fromIntegral (length ls)

-- > (atlag . filter (>= 4.5)) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- > atlag $ filter (< 4.5) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- > (take 4 . reverse . filter odd ) [1..20]
-- > take 4 . reverse . filter odd $ [1..20]
-- > take 4 ( reverse ( filter odd [1..20]))
-- > take 4 $ reverse $ filter odd $ [1..20]
-- ```

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista elemszámát, 2 módszerrel (myLength),
mylength [] = 0
mylength (x:xs) = 1 + mylength xs

mylength2 [] res = res
mylength2 (x:xs) res = mylength2 xs (res+1)

mylength3 ls = foldr (\x -> (+) 1) 0 ls

mylength4 ls = foldl (\db x -> db + 1) 0 ls


mylength5 ls res = foldr (\x res-> 1 +res) res ls
-- - összeszorozza a lista elemeit, 2 módszerrel (myProduct),

myproduct [] = 1
myproduct ls = x*myproduct xs
    where
        (x:xs) = ls

myproduct2 [] res = res
myproduct2 (x:xs) res = myproduct2 xs (res*x)

myproduct3 ls = foldr (\x -> (*) x) 1 ls

myproduct4 ls res = foldr (\x res -> (*) x res) res ls

myproduct5 ls = foldr1 (\x -> (*) x) ls

-- - meghatározza egy lista legkisebb elemét (myMinimum),
myminimum [x] = x
myminimum (x1:x2:xs) = if x1<x2 then myminimum (x1:xs) else myminimum (x2:xs)

myminimum2 (x1:x2:xs)
    |x1 < x2 = myminimum (x1 : xs)
    |otherwise = myminimum2 (x2:xs)

myminimum3 ls = foldr1 min ls

myminimum4 ls = minimum ls

-- - meghatározza egy lista legnagyobb elemét (myMaximum),

mymax [x] = x
mymax(x1:x2:xs) = if x1>x2 then myminimum (x1:xs) else myminimum (x2:xs)

mymax2 (x1:x2:xs)
    |x1 > x2 = myminimum (x1 : xs)
    |otherwise = myminimum2 (x2:xs)

mymax3 ls = foldr1 max ls

mymax4 ls = maximum ls
-- - meghatározza egy lista n-ik elemét (!!),

listan ls n = ls !! n

listan2 ls n    
    |null ls= error "ures"
    |n<0 = error "neg index"
    |length ls <= n = error "tulnagyindex"
    |otherwise = ls !! n


-- - egymásután fűzi a paraméterként megadott két listát (++),

listafuz ls1 ls2 = ls1 ++ ls2

-- - megállapítja egy listáról, hogy az palindrom-e vagy sem,

palindrom ls = ls == reverse ls

palindrom2 ls    
    |ls == reverse ls = "palindrom"
    |otherwise = "nem palindrom"

palindrom3 [] = True
palindrom3 [x] = True
palindrom3 ls = if head ls == last ls then palindrom3 ((init.tail) ls) else False


-- - meghatározza egy egész szám számjegyeinek listáját,
szjl x
    |x<0 = szjl (abs x)
    |x<10 = [x]
    |otherwise  = szjl (div x 10) ++ [mod x 10]

-- - a lista első elemét elköltözteti a lista végére,

esu [] = error "ures lista"
esu (x: xs) = xs ++ [x]

esu2 ls = tail ls ++[head ls]
-- - meghatározza egy egész elemű lista elemeinek átlagértékét,

-- - meghatározza egy 10-es számrendszerbeli szám p számrendszerbeli alakját,

decp x p 
    |x<p = [x]
    |otherwise = decp (div x p) p++ [mod x p]


-- - meghatározza egy p számrendszerben megadott szám számjegyei alapján a megfelelő 10-es számrendszerbeli számot.

pdec ls p = foldl (\hatvany x -> x + (p*hatvany)) 0 ls 

pdec2 x p = 
    let
        szamjegyek x
            |x<10 = [x]
            |otherwise = mod x 10 : szamjegyek(div x 10)
        szjindex = zip (szamjegyek x) [0..]
    in [szam * (p*hatvany)| (szam,hatvany)<-szjindex]



-- III. Alkalmazzuk a map függvényt a II.-nél megírt függvényekre.

-- IV. Írjunk egy Haskell függvényt, amely meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét.
als = [3 ,-2,5,-7]

x0=2
-- V. Ha adva van egy P pont koordinátája a kétdimenziós síkban, és adott az lsP pontok egy listája, írjunk egy Haskell függvényt, amely meghatározza azt az lsP-beli P1 pontot, amely legközelebb van a P ponthoz.
poli [] x = 0
poli (a:als) x = a + x*(poli als x)