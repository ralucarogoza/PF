--09 dec 2022

--Scrieti instante ale clasei Functor pentru tipurile de date descrise mai jos.

{-
class Functor f where
fmap : : ( a -> b ) -> f a -> f b
-}

newtype Identity a = Identity a
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a
instance Functor Pair where
    fmap f (Pair a1 a2) = Pair (f a1) (f a2)

data Constant a b = Constant b
instance Functor (Constant a) where
    fmap f (Constant b) = Constant (f b) 

data Two a b = Two a b
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b
instance Functor (Three' a) where
    fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

data Four'' a b = Four'' a a a b
instance Functor (Four'' a) where
    fmap f (Four'' a1 a2 a3 b1) = Four'' a1 a2 a3 (f b1)

data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
    fmap f (Finance) = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)




data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap g (DaWrappa fa ga) = DaWrappa (fmap g fa) (fmap g ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap g (IgnoringSomething fa gb) = IgnoringSomething fa (fmap g gb)

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
    fmap g (Notorious go ga gt) = Notorious go ga (fmap g gt)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
    fmap f (NoGoat) = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats a1 a2 a3) = MoreGoats (fmap f a1) (fmap f a2) (fmap f a3)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
    fmap f (Halt) = Halt
    fmap f (Print string a) = Print string (f a)
    fmap f (Read a) = Read (fmap f a)

