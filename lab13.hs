
{- Monada Maybe este definita in GHC.Base 

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing


instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor Maybe where              
  fmap f ma = pure f <*> ma   
-}

pos :: Int -> Bool
pos  x = if (x>=0) then True else False

fct :: Maybe Int ->  Maybe Bool
fct  mx =  mx  >>= (\x -> Just (pos x))

--2. Vrem să definim o functie care adună două valori de tip Maybe Int

addM :: Maybe Int -> Maybe Int -> Maybe Int
addM mx my = undefined

--3. Să se treacă în notatia do urmatoarele functii:
cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))

prod f xs ys = [f x y | x <- xs, y<-ys]

myGetLine :: IO String
myGetLine = getChar >>= \x ->
      if x == '\n' then
          return []
      else
          myGetLine >>= \xs -> return (x:xs)

--4. Să se treacă în notatia cu secventiere urmatoarea functie:

prelNo noin =  sqrt noin

ioNumber = do
     noin  <- readLn :: IO Float
     putStrLn $ "Intrare\n" ++ (show noin)
     let  noout = prelNo noin
     putStrLn $ "Iesire"
     print noout

ioNumber2 = (readLn :: IO Float) >>= (\noin -> putStrLn ("Intrare\n" ++ show noin) >> let noout = prelNo noin in putStrLn "Iesire" >> print noout)


data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN = undefined
showPersonA :: Person -> String
showPersonA = undefined

{-
showPersonN $ Person "ada" 20
"NAME: ada"
showPersonA $ Person "ada" 20
"AGE: 20"
-}

showPerson :: Person -> String
showPerson = undefined 

{-
showPerson $ Person "ada" 20
"(NAME: ada, AGE: 20)"
-}


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

mshowPersonN ::  Reader Person String
mshowPersonN = undefined
mshowPersonA ::  Reader Person String
mshowPersonA = undefined 
mshowPerson ::  Reader Person String
mshowPerson = undefined 
{-
runReader mshowPersonN  $ Person "ada" 20
"NAME:ada"
runReader mshowPersonA  $ Person "ada" 20
"AGE:20"
runReader mshowPerson  $ Person "ada" 20
"(NAME:ada,AGE:20)"
-}