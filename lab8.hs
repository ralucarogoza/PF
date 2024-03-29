--25 nov 2022

--1.
data Punct = Pt [Int]


--a) Să se scrie o instantă a clasei Show pentru tipul de date Punct, astfel încât lista coordonatelor sa fie afisată sub forma de tuplu.
f :: Punct -> String
f(Pt []) = ""
f(Pt [x]) = show x
f (Pt (x:xs)) = show x ++ "," ++ f (Pt xs)

instance Show Punct where 
    show(Pt []) = show "()" 
    show (Pt x) = "(" ++ f (Pt x)  ++ ")"
    
--b) Să se scrie o instanţă a clasei ToFromArb pentru tipul de date Punct astfel incat lista coordonatelor punctului sa coincidă cu frontiera arborelui.

data Arb = Vid | F Int | N Arb Arb
        deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a



instance ToFromArb Punct where
    fromArb Vid = Pt []
    fromArb (F a) = Pt [a] 
    fromArb (N a b) = Pt (c ++ d) --concatenez cele 2 liste si facc rez de tip Pt
            where 
                Pt c = fromArb a 
                Pt d = fromArb b
    toArb (Pt []) = Vid
    toArb (Pt [a]) = F a
    toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs) ) 

--2.
data Geo a = Square a | Rectangle a a | Circle a 
    deriving Show

class GeoOps g where
    perimeter :: (Floating a) => g a -> a
    area :: (Floating a) => g a -> a

--Să se instantieze clasa GeoOps pentru tipul de date Geo. Pentru valoarea pi există
--functia cu acelasi nume (pi).

instance GeoOps Geo where
    perimeter (Square a) = a *4
    perimeter (Rectangle a b) = 2*a + 2*b
    perimeter (Circle a) = 2*pi*a
    area (Circle a) = pi*a*a
    area (Square a) = a*a
    area (Rectangle a b) = a*b

--b) Să se instantieze clasa Eq pentru tipul de date Geo, astfel încât două figuri geometrice să fie egale dacă au perimetrul egal.
instance (Floating a, Eq a) => Eq (Geo a) where
    b == c = (perimeter b == perimeter c)
    --(Square b) == (Circle c) = ( (perimeter (Square b)) == (perimeter (Circle c))) 