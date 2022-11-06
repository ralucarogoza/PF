--28 oct 2022

--[[x..y]| x <- [1..5], y <- [1..5], x < y]
--[[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5],[2,3],[2,3,4],[2,3,4,5],[3,4],[3,4,5],[4,5]]

--1.Folosind numai metoda prin selectie definiti o functie factori.
factori :: Int -> [Int]
factori x = [y|y<-[1..x], x `mod` y == 0]

--2.Folosind functia factori, definiti predicatul prim n care întoarce True dacă si numai dacă n este număr prim.
prim :: Int -> Bool
prim x 
    | (factori x == [1, x]) = True
    | otherwise = False
--prim=length(prin n)==2

--3.Folosind numai metoda prin selectie si functiile definite anterior, definiti functia numerePrime.
numerePrime :: Int -> [Int]
numerePrime l = [x|x<-[2..l], prim x]

--zip [1..5] [1..3] => [(1,1),(2,2),(3,3)]

--4.Definiti functia myzip3 care se comportă asemenea lui zip dar are trei argumente:
myzip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
myzip3 [] b c = []
myzip3 a [] c = []
myzip3 a b [] = []
myzip3 (a:at) (b:bt) (c:ct) = (a, b, c) : zip3 at bt ct

--myzip32 :: [a] -> [b] -> [c] -> [(a,b,c)]
--myzip32 m n p = [(x,y,z)|((x,y),z)<-zip((zip m n) p)]

--myzip33 a b c=if(length a==0 || length b==0 || length c==0) then []
--                else (head a,head b, head c):myzip33(tail a, tail b, tail c)


--map ($ 3) [ ( 4 +) , (10 * ) , ( ^ 2) , sqrt ] fct aplicate pe 3
--[7.0,30.0,9.0,1.7320508075688772]


--map (`elem` [2,3]) [1,3,4,5]
--[False, True, False, False]


--5. Scrieti o functie generică firstEl care are ca argument o listă de perechi de tip (a,b) si întoarce lista primelor elementelor din fiecare pereche.
--        (fst, snd)
firstEl :: [(a,b)] -> [a]
firstEl = map fst --primul elem 
firstEl' :: [(a,b)] -> [a]
firstEl' = map (\(x,y)->x)

--6.Scrieti functia sumList care are ca argument o listă de liste de valori Int si întoarce lista sumelor elementelor din fiecare listă (suma elementelor unei liste de întregi se calculează cu functia sum).
sumList :: [[Int]] -> [Int]
sumList l = map sum l

--7.Scrieti o functie prel2 care are ca argument o listă de Int si întoarce o listă în care elementele pare sunt înjumătătite, iar cele impare sunt dublate:
prel2 :: [Int] -> [Int]
prel2 = map(\x -> if even x then x `div` 2 else x*2)

--8.Scrieti o functie care primeste ca argument un caracter si o listă de siruri, rezultatul fiind lista sirurilor care contin caracterul respectiv (folositi functia elem).
f :: Char -> [String] -> [String]
f c s = filter(c `elem`) s

--9.Scrieti o functie care primeste ca argument o listă de întregi si întoarce lista pătratelor numerelor impare.
patrate :: [Int] -> [Int]
patrate l = map (^2) (filter odd l)

--10.Scrieti o functie care primeste ca argument o listă de întregi si întoarce lista pătratelor numerelor din pozitii impare. Pentru a avea acces la pozitia elementelor folositi zip.
patrate2 :: [Int] -> [Int]
patrate2 l = map (^2) (map snd (filter (odd.fst) (zip [0..] l)))

--11.Scrieti o functie care primeste ca argument o listă de siruri de caractere si întoarce lista obtinută prin eliminarea consoanelor din fiecare sir.
numaiVocale :: [String] -> [String]
numaiVocale l = map (filter (`elem` "aeiouAEIOU")) l

--12.Definiti recursiv functiile mymap si myfilter cu aceeasi functionalitate ca si functiile predefinite.
mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = f x : map f xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f [] = []
myfilter f (x:xs) 
    | f x = x : myfilter f xs
    | otherwise = myfilter f xs