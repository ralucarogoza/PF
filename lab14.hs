--5.2.

newtype WriterLS a = Writer {runWriter :: (a, [String])}

instance  Monad WriterLS where
  return va = Writer (va, [])
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterLS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterLS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterLS () 
tell log = Writer ((), [log])
  
logIncrement :: Int  -> WriterLS Int
logIncrement x = do
    tell ("increment: " ++ show x)
    return (x + 1)


logIncrement2 :: Int -> WriterLS Int
logIncrement2 x = do
    y <- logIncrement x
    logIncrement y

--Definiti functia logIncrementN, care generalizează logIncrement2, astfel:

logIncrementN :: Int -> Int -> WriterLS Int
logIncrementN x n = do
    if n > 2 then
        do
            y <- logIncrement2 x 
            logIncrementN y (n - 2)
    else
        logIncrement2 x



--6.
data Person = Person { name :: String, age :: Int }

--6.1. Definiti functiile care afisează “frumos” numele s, i vârsta unei persoane.
showPersonN :: Person -> String
showPersonN p = "NAME: " ++ name p  

showPersonA :: Person -> String
showPersonA p = "AGE: " ++ show (age p)  

--6.2. Folosind functiile definite la punctul 6.1, definiti functia care afis,ează “frumos” toate datele unei persoane.
showPerson :: Person -> String
showPerson p = "(NAME: " ++ name p ++ ", AGE: " ++ show (age p) ++ ")"

--6.3 Folosind monada Reader (aveti implementarea instantelor în fisierul lab13.hs),
--definiti variante monadice pentru cele trei functii definite anterior, fără a folosi
--functiile definite anterior. Variantele monadice vor avea tipul
--mshowPersonN :: Reader Person String
--mshowPersonA :: Reader Person String
--mshowPerson :: Reader Person String

newtype Reader env a = Reader { runReader :: env -> a }


instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env


instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance Functor (Reader env) where              
  fmap f ma = pure f <*> ma   

ask :: Reader env env
ask = Reader id 

mshowPersonN ::  Reader Person String
mshowPersonN = do 
    p <- ask 
    return ("NAME: " ++ name p)

mshowPersonA ::  Reader Person String
mshowPersonA = do 
    p <- ask 
    return ("AGE: " ++ show (age p))

mshowPerson ::  Reader Person String
mshowPerson = do 
    p <- ask 
    return ("(NAME: " ++ name p ++ ", AGE: " ++ show (age p) ++ ")")