--14 oct 2022

--1.Să se scrie o functie poly2 care are patru argumente de tip Double, a,b,c,x si calculează a*xˆ2+b*x+c.
poly2 :: Double -> Double -> Double -> Double -> Double
poly2 a b c x = a*x*x+b*x+c


--2.Să se scrie o functie eeny care întoarce “eeny” pentru input par si “meeny” pentru input impar. Hint: puteti folosi functia even.
eeny :: Integer -> String
eeny x = if(rem x 2 == 0)
            then "par"
            else "impar"

--var2
--eeny x = if even x then "eeny"
--          else "meeny"


--3.Să se scrie o functie fizzbuzz care întoarce “Fizz” pentru numerele divizibile cu 3, “Buzz” pentru numerele divizibile cu 5 
--si “FizzBuzz” pentru numerele divizibile cu ambele. Pentru orice alt număr se întoarce sirul vid. 
--Pentru a calcula modulo a două numere puteti folosi functia mod. Să se scrie această functie în 2 moduri: 
--folosind if si folosind garzi(conditii).
fizzbuzz :: Integer -> String
fizzbuzz x = if(rem x 3 == 0 && rem x 5 == 0)
                then "FizzBuzz"
            else if(rem x 3 == 0)
                then "Fizz"
            else if(rem x 5 == 0)
                then "Buzz"
            else ""

fizzbuzz2 :: Integer -> String
fizzbuzz2 x 
    | rem x 3 == 0 && rem x 5 == 0 = "FizzBuzz"
    | rem x 3 == 0 = "Fizz"
    | rem x 5 == 0 = "Buzz"
    | otherwise = ""
            
-- mod e tot o functie cu 2 parametri


--4.Numerele tribonacci sunt definite de ecuatia
--Tn = 1 dacă n = 1
--     1 dacă n = 2
--     2 dacă n = 3
--     Tn−1 + Tn−2 + Tn−3 dacă n > 3
--Să se implementeze functia tribonacci atât cu cazuri cât si ecuational.
tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci n = tribonacci(n-1) + tribonacci(n-2) + tribonacci(n-3)

--5.Să se scrie o functie care calculează coeficientii binomiali, folosind recursivitate. Acestia sunt determinati folosind urmatoarele ecuatii.
--B(n,k) = B(n-1,k) + B(n-1,k-1)
--B(n,0) = 1
--B(0,k) = 0
binomial :: Integer -> Integer -> Integer
binomial 0 k = 0
binomial n 0 = 1
binomial n k = binomial (n-1) k + binomial(n-1) (k-1)

--6.Să se implementeze următoarele functii folosind liste:
--a)verifL - verifică dacă lungimea unei liste date ca parametru este pară
verifL :: [Int] -> Bool
verifL x = if(mod (length x) 2 == 0) --length x `mod` 2==0
                then True
                else False 
--b)takefinal - pentru o listă dată ca parametru si un număr n, întoarce lista cu ultimele n elemente. Dacă lista are mai putin de n elemente, 
--se intoarce lista nemodificată.
takefinal :: [Int] -> Int -> [Int]
takefinal x n = if (length x < n)
                    then x
                else drop ((length x) - n) x 

--c)remove - pentru o listă si un număr n se întoarce lista din care se sterge elementul de pe pozitia n.
remove :: [Int] -> Int -> [Int]
remove x n = take (n-1) x ++ (drop n x) 

-- take (n - 1) x ++ drop n x e suficient

--recursivitate
semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
    | even h = h `div` 2 : t'
    | otherwise = t'
    where t' = semiPareRec t

--7.Să se scrie urmatoarele functii folosind recursivitate:
--a)myreplicate - pentru un întreg n si o valoare v întoarce lista de lungime n ce are doar elemente egale cu v. Să se scrie si prototipul functiei.
myreplicate :: Int -> a -> [a]
myreplicate 0 v = []
myreplicate n v = v : myreplicate(n-1) v


--b)
--sumImp - pentru o listă de numere întregi, calculează suma valorilor impare. Să se scrie si prototipul functiei
sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (h:t)
    | even h = sumImp t
    | otherwise = h + sumImp t


--c)
--totalLen - pentru o listă de siruri de caractere, calculează suma lungimilor sirurilor care încep cu caracterul ‘A’.
totalLen :: [String] -> Int
totalLen [] = 0
totalLen (h:t)
    | head h == 'A' = length h + totalLen t
    | otherwise = totalLen t