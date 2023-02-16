--15 nov 2022

data Expr = Const Int -- integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
           deriving Eq

--1.1. Să se instantieze clasa Show pentru tipul de date Expr, astfel încât să se afiseze
--mai simplu expresiile.

instance Show Expr where
    show(Const x) = show x
    show(e1 :+: e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    show(e1 :*: e2) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"

exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)

--1.2. Să se scrie o functie evalExp :: Expr -> Int care evaluează o expresie determinând
--valoarea acesteia.

evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (e1 :+: e2) = (evalExp e1) + (evalExp e2)
evalExp (e1 :*: e2) = (evalExp e1) * (evalExp e2)  

test11 = evalExp exp1
test12 = evalExp exp2 
test13 = evalExp exp3 
test14 = evalExp exp4 

--1.3. Să se scrie o functie evalArb :: Tree -> Int care evaluează o expresie modelată sub formă de arbore, determinând valoarea acesteia.
data Operation = Add | Mult 
        deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
           deriving (Eq, Show)

evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add t1 t2) = evalArb(t1) + evalArb(t2) --t1 e de tip Tree, deci stie pe care ramura sa intre
evalArb (Node Mult t1 t2) = evalArb(t1) * evalArb(t2)

arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

test21 = evalArb arb1
test22 = evalArb arb2 
test23 = evalArb arb3 
test24 = evalArb arb4 

--1.4. Să se scrie o functie expToArb :: Expr -> Tree care transformă o expresie în arborele corespunzător.
expToArb :: Expr -> Tree
expToArb (Const x) = Lf x
expToArb (e1 :+: e2) = Node Add (expToArb e1) (expToArb e2)
expToArb (e1 :*: e2) = Node Mult (expToArb e1) (expToArb e2)


--In acest exercitiu vom exersa manipularea listelor si tipurilor de date prin
--implementarea catorva colectii de tip tabela asociativa cheie-valoare.
--Aceste colectii vor trebui sa aiba urmatoarele facilitati
--• crearea unei colectii vide
--• crearea unei colectii cu un element
--• adaugarea/actualizarea unui element intr-o colectie
--• cautarea unui element intr-o colectie
--• stergerea (marcarea ca sters a) unui element dintr-o colectie
--• obtinerea listei cheilor
--• obtinerea listei valorilor
--• obtinerea listei elementelor



--2.1. Adaugati definitii implicite (in functie de functiile celelalte) pentru
--a.keys
--b.values
--c.fromList

class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert :: Ord key => key -> value -> c key value -> c key value
  clookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  keys c = [fst x|x <- toList c]
  values :: c key value -> [value]
  values c = [snd x|x <- toList c]
  toList :: c key value -> [(key, value)] --transf colectia intr o lista de tupluri
  fromList :: Ord key => [(key,value)] -> c key value
  fromList [] = empty
  fromList (x:xs) = insert (fst x) (snd x) (fromList xs)

--2.2. Fie tipul listelor de perechi de forma cheie-valoare:
newtype PairList k v = PairList { getPairList :: [(k, v)] } deriving Show
--getPairList(pairlist l) = l scoate lista de tupluri
--ex PairList [(1,'a'),(2,'c')]



--PairList [(2,'a')] => PairList {getPairList = [(2,'a')]}
--PairList {getPairList = [(2,'a')]} => PairList {getPairList = [(2,'a')]}                 pot apela in ambele moduri si mi da la fel

--PairList [(2,'a')] => PairList {getPairList = [(2,'a')]}
--getPairList (PairList [(2,'a')])  => [(2,'a')]
--getPairList (PairList [(2,'a'),(2,'b')]) => [(2,'a'),(2,'b')]


--Faceti PairList instanta a clasei Collection.
instance Collection PairList where
    empty = PairList[]
    singleton k v = PairList [(k,v)] --ia un elem din colectie 
    --insert k v (PairList l) = PairList $ (k,v) :l -- cu var asta pot adauga mai multe elem diferite care au aceeasi cheie
    insert k v l = PairList $ (k,v) : (getPairList(delete k l)) -- sterg elem care are cheia respectiva si il adaug cu noua cheie
    delete k (PairList l) = PairList ([(a,b)|(a,b)<-l, a /= k])
    clookup k (PairList l) = lookup k l
    toList (PairList l) = l
    --toList (PairList l) = getPairList(PairList l)


--clookup 2 (PairList [(2,'a'),(3,'b')]) => Just 'a' deci cauta valoarea de la cheia 2 si returneaza un maybeee
--clookup 4 (PairList [(2,'a'),(3,'b')]) => Nothing

--2.3. Fie tipul arborilor binari de cautare (ne-echilibrati):
data SearchTree key value = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

--Faceti SearchTree instanta a clasei Collection.

instance Collection SearchTree where
    empty = Empty
    singleton k v = BNode (Empty) k (Just v) Empty
    insert k v Empty = BNode (Empty) k (Just v) Empty
    insert k v (BNode left key value right) 
        | k < key = BNode (insert k v left) key value right
        | k > key = BNode left key value (insert k v right)
        | otherwise = BNode left k (Just v) right
    clookup k Empty = Nothing
    clookup k (BNode left key value right)
        | k < key = clookup k right
        | k > key = clookup k left
        | otherwise = value
    delete k (BNode left key value right)
        | k < key = BNode (delete k left) key value right
        | k > key = BNode left key value (delete k right)
        | otherwise = BNode left key Nothing right
    toList Empty = []
    toList (BNode left key Nothing right) = toList left ++ toList right
    --toList (BNode left key value right) = (key, case value of Just val -> val) : toList left  ++ toList right -- am inserat aici cheie val stanga dreapta
    toList (BNode left key value right) = toList left  ++ [(key, case value of Just val -> val)] ++ toList right

--toList (BNode (BNode Empty 2 (Just 5) Empty) 4 (Just 3) (BNode Empty 6 (Just 4) Empty))