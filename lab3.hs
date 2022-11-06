--21 oct 2022
import Data.Char

--1.Sa se scrie o functie nrVocale care pentru o lista de siruri de caractere, calculeaza numarul total de vocale ce apar în cuvintele 
--palindrom. Pentru a verifica daca un sir e palindrom, puteti folosi functia reverse, iar pentru a cauta un element într-o lista puteti folosi 
--functia elem. Puteti defini oricâte functii auxiliare.
palindrom :: String -> Bool
palindrom x = reverse x == x

voc :: String -> Int
voc "" = 0
voc(c:s)
    | elem c "aeiouAEIOU" = 1 + voc s
    | otherwise = voc s

nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (h:t)
    | palindrom h = voc h + nrVocale t
    | otherwise = nrVocale t


--2.Sa se scrie o functie care primeste ca parametru un numar si o lista de întregi, si adauga elementul dat dupa fiecare element par din 
--lista. Sa se scrie si prototipul functiei.
paritate :: Int -> Bool
paritate x = if (rem x 2 == 0) 
                then True
                else False


f :: Int -> [Int] -> [Int]
f x [] = [] 
f x (h:t)
    | paritate h = h : x : (f x t)
    | otherwise = h : f x t


--Liste definite prin comprehensiune sau selectie
--[expresie|selectori, legari, filtrari]
--semiPareComp l=[x `div` 2|x <- l, even x]
--nrVocale l = sum[voc s| s<-l, s==reverse s]
--voc s=sum[1| c<-s, elem c "aeiouAEIOU"]


--3.Sa se scrie o functie care are ca parametru un numar întreg si determina lista de divizori ai acestui numar. Sa se scrie si prototipul functiei.
divizori :: Int -> [Int]
divizori x = [y|y<-[1..x], x `mod` y == 0]


--4.Sa se scrie o functie care are ca parametru o lista de numere întregi si calculeaza lista listelor de divizori.
listadiv :: [Int] -> [[Int]]
listadiv s = [divizori l|l <- s]


--5.Scrieti o functie care date fiind limita inferioara si cea superioara (întregi) a unui interval închis si o lista de numere întregi, calculeaza lista numerelor din lista care apartin intervalului.
--a)Folositi doar recursie. Denumiti functia inIntervalRec.
inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec x y [] = []
inIntervalRec x y (h:t)
    | elem h [x..y] = h : inIntervalRec x y t
    | otherwise = inIntervalRec x y t

--b)Folositi descrieri de liste. Denumiti functia inIntervalComp.
inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp x y l = [a|a <- l, elem a [x..y]]


--6.Scrieti o functie care numara câte numere strict pozitive sunt într-o lista data ca argument.
--a)Folositi doar recursie. Denumiti functia pozitiveRec.
pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (h:t)
    | h > 0 = 1 + pozitiveRec t
    | otherwise = pozitiveRec t
--b)Folositi descrieri de liste. Denumiti functia pozitiveComp.
pozitiveComp :: [Int] -> Int
pozitiveComp l = sum[1|a<-l, a > 0]


--7.Scrieti o functie care data fiind o lista de numere calculeaza lista pozitiilor elementelor impare din lista originala.
--a)Folositi doar recursie. Denumiti functia pozitiiImpareRec.
pozitiiImpareAux :: [Int] -> Int -> [Int]
pozitiiImpareAux [] _ = []
pozitiiImpareAux (h:t) i
    | odd h = i : pozitiiImpareAux t (i+1)
    | otherwise = pozitiiImpareAux t (i+1)

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec l = pozitiiImpareAux l 0

--b)Folositi descrieri de liste. Denumiti functia pozitiiImpareComp.
pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [poz|(el, poz) <- zip l [0..], mod el 2 == 1]
--(el, poz) <- zip l[0..]


--8.Scrieti o functie care calculeaza produsul tuturor cifrelor care apar în sirul de caractere dat ca intrare. Daca nu sunt cifre în sir, raspunsul functiei trebuie sa fie 1 .
--a)Folositi doar recursie. Denumiti functia multDigitsRec.
multDigitsRec :: String -> Int 
multDigitsRec [] = 1
multDigitsRec (h:t)
    | isDigit h = (digitToInt h) * multDigitsRec t 
    | otherwise = multDigitsRec t 

--b)Folositi descrieri de liste. Denumiti functia multDigitsComp
multDigitsComp :: String -> Int
multDigitsComp s = product[digitToInt c|c <- s, isDigit c]