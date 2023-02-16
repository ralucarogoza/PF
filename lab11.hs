--fmap <$> :: (a -> b) -> m a -> m b
-- <*> :: m(a -> b) -> m a -> m b
-- pure :: a -> m a


--1. Să se scrie instante Functor si Applicative pentru tipul de date List.
data List a = Nil
            | Cons a (List a)
        deriving (Eq, Show)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs) -- aplic mai departe functia pe restul listei folosind fmap

concatenare :: List a -> List a -> List a
concatenare Nil b = b
concatenare a Nil = a
concatenare (Cons a l) b = Cons a (concatenare b l)

-- concatenare (Cons 1 Nil) (Cons 2 (Cons 7 Nil))
-- Cons 1 (Cons 2 (Cons 7 Nil))

instance Applicative List where
    pure x = Cons x Nil 
    Nil <*> x = Nil
    (Cons f xs) <*> (Cons x l) = Cons (f x) (concatenare (fmap f l) (xs <*> Cons x l))
--aplic fct din prima lista pe primul elem din a doua lista
--in concatenare intai pun aplicarea functiei pe urmatoarele elem din lista a doua pana termin cu ele
--apoi iau urmatoarea functie si o aplic din nou pe toate
--xs reprezinta restul de functii pe care le aplic pe TOATE elementele din lista a doua


f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
test1 = (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

--2.
data Cow = Cow {
        name :: String
        , age :: Int
        , weight :: Int
        } deriving (Eq, Show)

--a) Să se scrie functiile noEmpty, respectiv noNegative care valideaza un string, respectiv un intreg.
noEmpty :: String -> Maybe String
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative x 
    | x < 0 = Nothing
    | x > 0 = Just x

--b) Sa se scrie o functie care construieste un element de tip Cow verificând numele, varsta si greutatea cu functiile de la a).
cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString n a w
    | noEmpty n == Just n && noNegative a == Just a && noNegative w == Just w = Just Cow{name = n, age = a, weight = w}
    | otherwise = Nothing

test24 = cowFromString "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})

--c) Sa se scrie functia de la b) folosind fmap si <*>.
--Cow <$> noEmpty nume :: Maybe(Int -> Int -> Cow)
--apoi noNegative age

cow :: String -> Int -> Int -> Maybe Cow
cow n a w = Cow <$> noEmpty n <*> noNegative a <*> noNegative w

test25 = cow "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})


--3.
newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address
    deriving (Eq, Show)

--a)Să se implementeze o functie validateLength care validează lungimea unui sir (sa fie mai mică decât numărul dat ca parametru)
validateLength :: Int -> String -> Maybe String
validateLength n s 
    | length s < n = Just s 
    | otherwise = Nothing

test31 = validateLength 5 "abc" == Just "abc"

--b)Să se implementeze functiile mkName si mkAddress care transformă un sir de caractere într-un element din tipul de date asociat, validând stringul cu functia validateLength (numele trebuie sa aiba maxim 25 caractere iar adresa maxim 100).
mkName :: String -> Maybe Name
mkName s
    | validateLength 26 s == Just s = Just (Name s) 
    | otherwise = Nothing

mkAddress :: String -> Maybe Address
mkAddress s
    | validateLength 101 s == Just s = Just (Address s)
    | otherwise = Nothing

test32 = mkName "Gigel" ==  Just (Name "Gigel")
test33 = mkAddress "Str Academiei" ==  Just (Address "Str Academiei")

--c)Să se implementeze functia mkPerson care primeste ca argument două siruri de caractere si formeaza un element de tip Person daca sunt validate conditiile, folosind functiile implementate mai sus.
mkPerson :: String -> String -> Maybe Person
mkPerson name address
    | mkName name == Just (Name name) && mkAddress address == Just (Address address) = Just (Person (Name name) (Address address))
    | otherwise = Nothing

test34 = mkPerson "Gigel" "Str Academiei" == Just (Person (Name "Gigel") (Address "Str Academiei"))

mname s = Name <$> validateLength 26 s

--d)Să se implementeze functiile de la b) si c) folosind fmap si <*>.
nameP :: String -> Maybe Name
nameP n = Name <$> validateLength 26 n 

addressP :: String -> Maybe Address
addressP a = Address <$> validateLength 101 a

person :: String -> String -> Maybe Person
person n a = Person <$> nameP n <*> addressP a

test36 = person "Gigel" "Str Academiei" == Just (Person (Name "Gigel") (Address "Str Academiei"))