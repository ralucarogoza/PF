--7 oct 2022

--Suma a doua numere
double :: Integer -> Integer
double x = x+x

--Suma a trei numere
triple :: Integer -> Integer
triple x = x+x+x

--maximul dintre 2 numere
maxim :: Integer -> Integer -> Integer
maxim x y = if (x > y)
                then x
                else y

--maximul dintre 3 numere
maxim3 x y z = maxim x (maxim y z)

maxim32 :: Integer -> Integer -> Integer -> Integer
maxim32 x y z = if(x > y && x>z)
                    then x
                    else if(y > x && y > z)
                            then y
                            else z

--maximul dintre 4 numere
maxim4 :: Integer -> Integer -> Integer -> Integer -> Integer
maxim4 x y z t = let
                    a = maxim x y 
                    b = maxim z t
                in if a > b then a else b

--6. Să se scrie următoarele functii:
--a) functie cu 2 parametri care calculeaza suma pătratelor celor două numere;
f :: Integer -> Integer -> Integer
f x y = x*x + y*y

--b) functie cu un parametru ce întoarce mesajul “par” dacă parametrul este par si “impar” altfel;
paritate :: Integer -> [Char]
paritate x = if (rem x 2 == 0) --mod x 2
                then "par"
                else "impar"

--c) functie care calculează factorialul unui număr;
fact :: Integer -> Integer
fact 0 = 1
fact x = x * fact(x-1)

--d) functie care verifică dacă un primul parametru este mai mare decât dublul celui de-al doilea parametru.
g :: Integer -> Integer -> Bool
g x y = if(x > y*2) 
            then True
            else False
--sau g x y = x > y*2
