--04.11.2022
--foldr & foldl

--foldr(\h t -> (h+1):t)
--[5] [1,2,3] => (1+1):((2+1):((3+1):[5])) => [2,3,4,5]

--1.Calculati suma pătratelor elementelor impare dintr-o listă dată ca parametru.
sumaPatrate :: [Int] -> Int
sumaPatrate l = foldr (+) 0 (map (^2) (filter odd l))

--2.Scrieti o functie care verifică faptul că toate elementele dintr-o listă sunt True, folosind foldr.
verifica :: [Bool] -> Bool
verifica l = foldr (&&) True l

--3.Scrieti o functie care verifică dacă toate elementele dintr-o listă de numere întregi satisfac o proprietate dată ca parametru.
allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies p l = foldr (&&) True (map p l)

--4.Scrieti o functie care verifică dacă există elemente într-o listă de numere întregi care satisfac o proprietate dată ca parametru.
anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies p l = foldr (||) True (map p l)

--5.Redefiniti functiile map si filter folosind foldr. Le puteti numi mapFoldr si filterFoldr.
mapFoldr ::  (a -> b) -> [a] -> [b]
mapFoldr f l = foldr(\x xs -> (f x):xs) [] l

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f l = foldr(\x xs -> if f x then x:xs else xs) [] l

--6.Folosind functia foldl, definiti functia listToInt care transformă o lista de cifre (un număr foarte mare stocat sub formă de listă) în numărul intreg asociat. Se presupune ca lista de intrare este dată corect.
listToInt :: [Integer] -> Integer
listToInt l = foldl (\x xs -> (x*10)+xs) 0 l 

--7.(a) Scrieti o functie care elimină un caracter dintr-un sir de caractere.
rmChar :: Char -> String -> String
rmChar c s = foldr(\x xs -> if c == x then xs else x:xs) "" s

--(b) Scrieti o functie recursivă care elimină toate caracterele din al doilea argument care se găsesc în primul argument, folosind rmChar.
rmCharsRec :: String -> String -> String
rmCharsRec "" a = a
rmCharsRec a "" = ""
rmCharsRec a (b:bs)        
    |  b `elem` a = rmCharsRec a (rmChar b (b:bs))
    | otherwise = b : rmCharsRec a bs

--(c)Scrieti o functie echivalentă cu cea de la (b) care foloseste rmChar si foldr în locul recursiei.
rmCharsFold :: String -> String -> String
rmCharsFold s1 s2 = foldr rmChar s2 s1

--rmChar 'a' s2 => "fotbl"
--rmChar 'b' "fotbl" => "fotl"
--foldr => ('a' rmChar('b' rmChar(...'l' rmChar s2)))
--rmCharFold ch s = foldr(\x xs -> rmChar x xs) s ch
--var2
--rmCharsFold a b = foldr(\h t -> if(a == rmChar h a) then h:t else t) "" b
