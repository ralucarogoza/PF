--29 nov 2022

import Data.List (nub)
import Data.Maybe (fromJust)


type Nume = String
data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop
  | Prop :->: Prop
  | Prop :<->: Prop
  deriving (Eq, Read)
infixr 2 :|:
infixr 3 :&:

--1. Scrieti următoarele formule ca expresii de tip Prop, denumindu-le p1, p2, p3.
--1) (P ∨ Q) ∧ (P ∧ Q)

p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

--2) (P ∨ Q) ∧ (¬P ∧ ¬Q)

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))

--3) (P ∧ (Q ∨ R)) ∧ ((¬P ∨ ¬Q) ∧ (¬P ∨ ¬R))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")))

--2. Faceti tipul Prop instantă a clasei de tipuri Show, înlocuind conectivele Not, :|: si :&: cu ~, |si & si folosind direct numele variabilelor în loc de constructia Var nume.

instance Show Prop where
  show (Var x) = x
  show (F) = "False"
  show (T) = "True"
  show (Not p) = "(~" ++ show p ++ ")"
  show (p :|: q) = "(" ++ show p ++ "|" ++ show q ++ ")"
  show (p :&: q) = "(" ++ show p ++ "&" ++ show q ++ ")"
  show (p :->: q) = "(" ++ show p ++ "->" ++ show q ++ ")"
  show (p :<->: q) = "(" ++ show p ++ "<->" ++ show q ++ ")"

test_ShowProp :: Bool
test_ShowProp =
    show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"

--3. Definiti o functie eval care dat fiind o expresie logică si un mediu de evaluare, calculează valoarea de adevăr a expresiei.
type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

--data.Maybe pt fromJust
--fromJust (Just 3) => 3

-- impureLookup 1 [(1,'A'),(2,'B'),(3,'C')] => 'A'


eval :: Prop -> Env -> Bool
eval T _ = True
eval F _ = False
eval (Var x) l = impureLookup x l 
eval (Not p) l = not (eval p l)
eval (p :|: q) l = (eval p l) || (eval q l)
eval (p :&: q) l = (eval p l) && (eval q l) 
eval (p :->: q) l = not (eval p l) || (eval q l)  -- (~p) | ~(~q)
eval (p :<->: q) l = ((eval (p :->: q) l) && (eval (q :->: p) l))

test_eval = eval  (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True

--4. Definiti o functie variabile care colectează lista tuturor variabilelor dintr-o formulă. Indicatie: folositi functia nub.

variabile :: Prop -> [Nume]
variabile (Var x) = [x]
variabile T = []
variabile F = []
variabile (Not p) = variabile p
variabile (p :|: q) = nub $ variabile p ++ variabile q
variabile (p :&: q) = nub $ variabile p ++ variabile q
variabile (p :->: q) = nub $ variabile p ++ variabile q 
variabile (p :<->: q) = nub $ variabile p ++ variabile q

--nub "AAAAAAAAAAAABBBBBBBBBBBBBBCCCCC" => "ABC"

test_variabile =
  variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]


--5. Dată fiind o listă de nume, definiti toate atribuirile de valori de adevăr posibile pentru ea.

envs :: [Nume] -> [Env]
envs l = [zip l y| y<-sequence(take (length l) (repeat [True, False]))]

--Main> sequence (take 2 (repeat [True, False]))
--[[True,True],[True,False],[False,True],[False,False]]
--Main> (take 2 (repeat [True, False]))
--[[True,False],[True,False]]

test_envs = 
    envs ["P", "Q"]
    ==
    [ [ ("P",False)
      , ("Q",False)
      ]
    , [ ("P",False)
      , ("Q",True)
      ]
    , [ ("P",True)
      , ("Q",False)
      ]
    , [ ("P",True)
      , ("Q",True)
      ]
    ]

--6. Definiti o functie satisfiabila care dată fiind o Propozitie verifică dacă aceasta este satisfiabilă. Puteti folosi rezultatele de la exercitiile 4 si 5.

satisfiabila :: Prop -> Bool
satisfiabila p = foldr (||) False [eval p l| l<-envs (variabile p)]

test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False
--foldr (||) True [eval (Not (Var "P") :&: Var "Q") l| l<-[[("P", False), ("Q", True)], [("P", True), ("Q", False)]]]
--foldr (||) False [eval (Not (Var "P") :&: Var "P") l| l<-[[("P",True),("P",True)],[("P",True),("P",False)],[("P",False),("P",True)],[("P",False),("P",False)]]]

--7. O propozitie este validă dacă se evaluează la True pentru orice interpretare a varibilelor. O forumare echivalenta este aceea că o propozitie este validă dacă negatia ei este nesatisfiabilă. Definiti o functie valida care verifică dacă o propozitie este validă.

valida :: Prop -> Bool
valida p = foldr (&&) True [eval p l| l<-envs (variabile p)]

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True

--9. Extindeti tipul de date Prop si functiile definite până acum pentru a include conectivele logice -> (implicatia) si <-> (echivalent, a), folosind constructorii :->: si :<->:. 

--10. Două propozitii sunt echivalente dacă au mereu aceeasi valoare de adevăr, indiferent de valorile variabilelor propozitionale. Scrieti o functie care verifică dacă două propozitii sunt echivalente.

echivalenta :: Prop -> Prop -> Bool
echivalenta p q = valida (p :<->: q)
 
test_echivalenta1 = True == (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))
test_echivalenta2 = False == (Var "P") `echivalenta` (Var "Q")
test_echivalenta3 = True == (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))