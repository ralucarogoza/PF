--11 nov 2022

--type NumeA = String
--data Animal = Pisica NumeA | Caine NumeA Rasa


--1.
data Fruct = Mar String Bool
            | Portocala String Int
--a. Scrieti o functie care indică dacă un fruct este o portocală de Sicilia sau nu. Soiurile de portocale din Sicilia sunt Tarocco, Moro si Sanguinello.
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Mar _ _) = False
ePortocalaDeSicilia (Portocala soi _) 
        | elem soi ["Tarocco", "Moro", "Sanguinello"] = True
        | otherwise = False

--ePortocalaDeSicilia(Portocala soi _) = soi `elem` [...]
--ePortocalaDeSicilia _ = False

--b. Scrieti o functie care calculează numărul total de felii ale portocalelor de Sicilia dintr-o listă de fructe.
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia (Mar _ _ : t) = nrFeliiSicilia t
nrFeliiSicilia [] = 0
nrFeliiSicilia (Portocala a b : t) = b + nrFeliiSicilia t

listaFructe = [Mar "Ionatan" False, Portocala "Sanguinello" 10, Portocala "Valencia" 22, Mar "Golden Delicious" True, Portocala "Sanguinello" 15, Portocala "Moro" 12, Portocala "Tarocco" 3, Portocala "Moro" 12, Portocala "Valencia" 2, Mar "Golden Delicious" False, Mar "Golden" False, Mar "Golden" True]


--c. Scrieti o functie care calcuelază numărul de mere care au viermi dintr-o lista de fructe.
nrMereViermi :: [Fruct] -> Int
nrMereViermi [] = 0
nrMereViermi (Portocala _ _ : t) = nrMereViermi t
nrMereViermi (Mar a b : t)
    | b == True = 1 + nrMereViermi t
    | otherwise = nrMereViermi t

--2.
type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

--a.Scrieti o functie care întoarce "Meow!" pentru pisică si "Woof!" pentru câine.
vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!" 

--b. Scrieti o functie care întoarce rasa unui câine dat ca parametru sau Nothing dacă parametrul este o pisică.
rasa :: Animal -> Maybe String
rasa (Pisica _) = Nothing
rasa (Caine a b) = Just b 

--3.Se dau urmatoarele tipuri de date ce reprezintă matrici cu linii de lungimi diferite:
data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

--a. Scrieti o functie care verifica daca suma elementelor de pe fiecare linie este egala cu o valoare n. Rezolvati cerinta folosind foldr.
sumeLinii :: Matrice -> [Int]
sumeLinii (M []) = []
sumeLinii (M ((L x):xs)) = sum x : sumeLinii (M (xs))

verifica :: Matrice -> Int -> Bool
verifica (M l) n = foldr (&&) True (foldr (\suma acumulator -> if suma == n then True : acumulator else False : acumulator) [] (sumeLinii (M l)))

test_verif1 = verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10 
test_verif2 = verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25 

--b.Scrieti o functie doarPozN care are ca parametru un element de tip Matrice si un numar intreg n, si care verifica daca toate liniile de lungime n din matrice au numai elemente strict pozitive.

elemStrictPoz :: Matrice -> Int -> [Bool]
elemStrictPoz (M []) _ = []
elemStrictPoz (M (L x:xs)) n 
    | (length x /= n) && ((filter (>0) x) == x) = True : (elemStrictPoz(M (xs)) n)
    | (length x == n)  && ((filter (>0) x) == x) = True : (elemStrictPoz(M (xs)) n)
    | otherwise = False : (elemStrictPoz(M (xs)) n)

doarPozN :: Matrice -> Int -> Bool
doarPozN (M l) n = foldr (&&) True (elemStrictPoz (M l) n)

testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3
testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 

--c.Definiti predicatul corect care verifică dacă toate liniile dintr-o matrice au aceeasi lungime.
lungimiLinii :: Matrice -> [Int]
lungimiLinii (M []) = []
lungimiLinii (M ((L x):xs)) = length x : lungimiLinii(M (xs))

corect :: Matrice -> Bool
corect (M l) = foldr (&&) True (foldr (\x xs -> if x == (head(lungimiLinii(M l))) then True : xs else False : xs) [] (lungimiLinii(M l)))


-- corect :: Matrice -> Bool
-- corect (M l) = foldr (&&) True (foldr (\lung acumulator -> if lung == (head(lungimiLinii(M l))) then True : acumulator else False : acumulator) [] (lungimiLinii(M l)))

testcorect1 = corect (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 
testcorect2 = corect (M[L[1,2,3], L[4,5,8], L[3,6,8], L[8,5,3]]) 


