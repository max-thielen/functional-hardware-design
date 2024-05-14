{-# OPTIONS_GHC -Wno-orphans #-}

module Lab1 where 
import Clash.Prelude

-- ex1:
applyTwice :: (a -> a) -> a -> a
applyTwice f = f . f

-- ghci> applyTwice (\x -> x + 2) 3
-- 7

-- ex2:
foo :: Bool -> a -> a -> a
foo cond f g
  | cond = f
  | otherwise = g

-- ex3:
foo2 :: a -> a
foo2 = \x -> x

bar :: Integer -> Integer -> Integer
bar = \x y -> x + y

-- ex4:
foo3 :: (a -> b) -> (c -> a) -> c -> b
foo3 f g = f . g

-- ex5
myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f b a = f a b

-- ex6:
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = [f x | x <- xs]

-- ex7:
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = [x | x <- xs, f x]

-- ex8:
myZipWith :: [a] -> [b] -> [(a, b)]
myZipWith [] _ = []
myZipWith _ [] = []
myZipWith (x:xs) (y:ys) = (x, y) : myZipWith xs ys

-- ex9:
mulTwo :: (Num a) => [a] -> [a]
mulTwo xs = myMap (\x -> x * 2) xs

-- ex10:
midPoint :: forall a . (Bounded a, Integral a) => a
midPoint = ((minBound :: a) + (maxBound :: a)) `div` 2

upperElements :: forall a. (Bounded a, Integral a) => [a] -> [a]
upperElements xs = myFilter (\x -> x > midPoint) xs

-- ex11:
zipAdd :: Num a => [a] -> [a] -> [a]
zipAdd xs ys = [x + y | (x,y) <- (myZipWith xs ys)]

-- ex12:
greaterFive :: Integral a => [a] -> [a]
greaterFive xs = myFilter (>5) xs

-- ex13:
mulTuples :: Num a => [(a,a)] -> [a]
mulTuples xs = myMap (\(x,y) -> x * y) xs