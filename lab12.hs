--23 dec 2022
import Data.Monoid

data BinaryTree a = Leaf a | Node ( BinaryTree a ) ( BinaryTree a )
    deriving Show

foldTree :: ( a -> b -> b ) -> b -> BinaryTree a -> b
foldTree f i ( Leaf x ) = f x i
foldTree f i (Node l r ) = foldTree f ( foldTree f i r ) l

instance Foldable BinaryTree where
    foldr = foldTree
none = Nothing
one = Just 3
tree = Node(Node( Leaf 1) ( Leaf 2) ) (Node ( Leaf 3) ( Leaf 4) )


--1. Implementati următoarele functii folosind foldMap si/sau foldr din clasa Foldable,
--apoi testati-le cu mai multe tipuri care au instantă pentru Foldable


elem1 :: (Foldable t, Eq a) => a -> t a -> Bool
elem1 e l = foldr (\x xs -> x == e || xs) False l

-- elem1 3 (Just 5)        
-- False
-- ghci> elem1 3 [3,4]   
-- True

elem2 :: (Foldable t, Eq a) => a -> t a -> Bool
elem2 e l = getAny $ foldMap (\x -> Any (x == e)) l

null1 :: (Foldable t) => t a -> Bool
null1 e = foldr (\x xs -> False) True e

null2 :: (Foldable t) => t a -> Bool
null2 e = not (getAny $ foldMap (\x -> Any True) e)
-- mergea si cu All (False) dar e corect!

length1 :: (Foldable t) => t a -> Int
length1 e = foldr (\x -> (1+)) 0 e

length2 :: (Foldable t) => t a -> Int
length2 e = getSum $ foldMap (\x -> 1) e

toList1 :: (Foldable t) => t a -> [a]
toList1 e = foldr (\x xs -> x : xs) [] e

toList2 :: (Foldable t) => t a -> [a]
toList2 e = foldMap (\x -> [x]) e

fold1 :: (Foldable t, Monoid m) => t m -> m
fold1 e = foldMap id e


--2. Scrieti instante ale lui Foldable pentru următoarele tipuri, implementand functia foldMap.

data Constant a b = Constant b
instance Foldable (Constant a) where
    foldMap f (Constant b) = f b

exConst = foldMap Sum (Constant 3)
exConst2 = foldMap Any (Constant False)

data Two a b = Two a b
instance Foldable (Two a) where
    foldMap f (Two a b) = f b 

exTwo = foldMap Sum (Two 1 6)

data Three a b c = Three a b c
instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

exThree = foldMap Sum (Three 1 2 3)

data Three' a b = Three' a b b
instance Foldable (Three' a) where
    foldMap f (Three' a b1 b2) = f b1 <> f b2 

exThree' = foldMap Sum (Three' 1 2 3)

data Four' a b = Four' a b b b
instance Foldable (Four' a) where
    foldMap f (Four' a b1 b2 b3) = f b1 <> f b2 <> f b3

exFour' = foldMap Sum (Four' 4 5 6 7)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Foldable GoatLord where
    foldMap f NoGoat = mempty
    foldMap f (OneGoat a) = f a
    foldMap f (MoreGoats a1 a2 a3) = foldMap f a1 <> foldMap f a2 <> foldMap f a3

exGoat = foldMap Sum (MoreGoats (OneGoat 4) NoGoat (MoreGoats (OneGoat 3) (OneGoat 6) NoGoat))
exGoat2 = foldr (*) 1 (MoreGoats (OneGoat 4) NoGoat (MoreGoats (OneGoat 3) (OneGoat 6) NoGoat))
