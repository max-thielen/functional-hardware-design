module Exercises where
import Clash.Prelude

-- Lab 1:

-- ex1:
data Pair a = Pair a a
    deriving Show 

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

-- fmap (+1) (Pair 1 2)
-- Pair 2 3

-- ex2:
data MyTuple a b = MyTuple a b
    deriving Show

instance Functor (MyTuple a) where
    fmap f (MyTuple x y) = MyTuple (x) (f y)

-- ghci> fmap (+1) (MyTuple 1 2)
-- MyTuple 1 3

-- ex3:
data MyList a = Empty | Element a (MyList a)
    deriving (Show)

-- ghci> emptyList = Empty :: MyList Int
-- ghci> emptyList
-- Empty
-- ghci> myList = Element 1 (Element 2 (Element 3 Empty)) :: MyList Int
-- ghci> myList   
-- Element 1 (Element 2 (Element 3 Empty))

instance Functor MyList where
    fmap _ Empty = Empty
    fmap f (Element x xs) = Element (f x) (fmap f xs)

-- ghci> fmap (*2) myList
-- Element 2 (Element 4 (Element 6 Empty))

-- ex4:
data MyBinaryTree a = Leaf | Node a (MyBinaryTree a) (MyBinaryTree a)
    deriving (Show)

-- ghci> trunk = Leaf :: MyBinaryTree Int
-- ghci> trunk
-- Leaf
-- ghci> tree = Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf) :: MyBinaryTree Int
-- ghci> tree
-- Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)

instance Functor MyBinaryTree where
    fmap _ Leaf = Leaf
    fmap f (Node n left right) = Node (f n) (fmap f left) (fmap f right)

-- ghci> fmap (*2) tree
-- Node 2 (Node 4 Leaf Leaf) (Node 6 Leaf Leaf)

-- ex5:
instance Applicative Pair where
    pure x = Pair x x
    Pair f g <*> Pair x y = Pair (f x) (g y)

-- ghci> pure 2 :: Pair Int
-- Pair 2 2
-- ghci> Pair (+1) (*2) <*> Pair 1 2
-- Pair 2 4

-- ex6:
instance Applicative MyList where
    pure x = Element x Empty
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    Element f fs <*> Element x xs = Element (f x) (fs <*> xs)

-- ghci> pure 2 :: MyList Int
-- Element 2 Empty
-- ghci> (*) <$> (Element 2 (Element 4 Empty)) <*> (Element 3 (Element 5 Empty)) :: MyList Int
-- Element 6 (Element 20 Empty)
-- ghci> (*) <$> (Element 2 (Element 4 Empty)) <*> Empty :: MyList Int
-- Empty

-- ex7:
instance Applicative MyBinaryTree where
    pure x = Node x Leaf Leaf
    Leaf <*> _ = Leaf
    _ <*> Leaf = Leaf
    Node f l r <*> Node x left right = Node (f x) (l <*> left) (r <*> right)

-- ex8:
-- Applicative for MyTyple (a, b) is trivial since with pure given one argument cannot make two types
-- and since Functor for MyTuple can only manipulate the second varible in MyTuple <*> wouldnt know what to do with the first two given vars in each tuple

-- instance Applicative (MyTuple a) where
--     pure x = MyTuple _ x
--     MyTuple f g <*> MyTuple x y = MyTuple (f x) (g y)

-- from hackage:
-- instance Monoid a => Applicative ((,) a) where
--     pure x = (mempty, x)
--     (u, f) <*> (v, x) = (u <> v, f x)
--     liftA2 f (u, x) (v, y) = (u <> v, f x y)