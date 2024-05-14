{-# OPTIONS_GHC -Wno-orphans #-}

module Lab2 where
import Clash.Prelude as Prelude
import Data.List as List ( (++), length, drop, take, mapAccumL )

-- ex14:
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ acc [] = acc
myFoldl f acc (b:bs) = myFoldl f (f acc b) bs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (b:bs) = f b $ myFoldr f acc bs

-- ex15:
myScanl :: (a -> b -> a) -> a -> [b] -> [a]
myScanl _ acc [] = [acc]
myScanl f acc (b:bs) = acc : myScanl f (f acc b) bs

-- ex16:
mySum :: Num a => [a] -> a
mySum = myFoldl (\acc x -> acc + x) 0
-- also: mySum xs = myFoldl (\x y -> x * y) 0 xs
-- infers that myFoldl's third argument is 'xs' (only list?)

-- ghci> mySum [1..3]
-- 6
-- ghci> mySum [1..10]
-- 55

-- ex17:
myProduct ::  Num a => [a] -> a
myProduct = myFoldl (\acc x -> acc * x) 0

-- ex18:
myLength ::  [a] -> Int
myLength = myFoldl (\acc _ -> acc + 1) 0

-- ex19:
myReverse ::  [a] -> [a]
myReverse = myFoldl (\acc x -> x:acc) []

-- ex20:
myMaximum :: Ord a => [a] ->  a
myMaximum [] = error "Empty List"
myMaximum (b:bs) = myFoldl (\acc x -> if x > acc then x else acc) b bs

--ex21:
myMapMaybe :: (a -> Maybe b) -> [a] -> [b]
myMapMaybe _ [] = []
myMapMaybe f (x:xs) = case f x of
                        Just y  -> y : myMapMaybe f xs
                        Nothing -> myMapMaybe f xs

-- ghci> myMapMaybe (\x -> if x > 5 then (Just x) else Nothing) [1..10]
-- [6,7,8,9,10]

--ex22:
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = [x | x <- xs, f x]

countOccurences :: Eq a => [a] -> a -> Int
countOccurences xs m = myLength $ myFilter (\x -> x == m) xs

-- ex23:
filterIncreasing :: Ord a => [a] -> [a]
filterIncreasing [] = []
filterIncreasing [x] = [x]
filterIncreasing (x:y:xs)
    | y >= x = x : filterIncreasing (y:xs)
    | otherwise = filterIncreasing (x:xs)

-- ghci> filterIncreasing [7,6,7,8,9,10]
-- [7,7,8,9,10]
-- ghci> filterIncreasing [1..10]
-- [1,2,3,4,5,6,7,8,9,10]
-- ghci> filterIncreasing [10..1]
-- []

-- ex24:
-- getWindow :: [a] -> Int -> Int -> [a]
-- getWindow xs start range = List.take (range) (List.drop start xs)

-- getAvg :: (Fractional a) => [a] -> a
-- getAvg xs = sum xs / fromIntegral (List.length xs)

-- getMovingAvg :: (Fractional a) => [a] -> Int -> Int -> [a] -> [a]
-- getMovingAvg input range index avg
--     | List.length input < range + index = avg
--     | List.length avg < range - 1 = getMovingAvg input range index (avg List.++ [0])
--     | otherwise = getMovingAvg input range (index + 1) (avg List.++ [getAvg (getWindow input index range)])


--  
-- get xs range = snd (List.mapAccumL
--                     (\avg index ->
--                         let window = List.take range (List.drop index xs)
--                             average = sum window `div` range
--                         in (avg List.++ [average], index + 1)
--                     )
--                     []
--                     [0..List.length xs - range])
-- get xs range = snd $ List.mapAccumL
--                     (\avg index ->
--                         let window' = List.take (fromIntegral range) (List.drop index xs)
--                             average = fromIntegral (sum window') / fromIntegral range
--                         in (avg List.++[average], fromIntegral avg)
--                         -- ((avg -List.++ [sum (List.take (range) (List.drop index xs)) / fromIntegral range]), index+1)
--                     )
--                     -- (\a b -> (a + b, a))
--                     range
--                     xs

set :: [Int] -> Int -> (Int, [Int])
set xs val = List.mapAccumL (\a b -> (a + b, a)) val xs

getMovingAvg :: (Fractional a) => [a] -> Int -> Int -> [a] -> [a]
getMovingAvg input range index avg
    | List.length input < range + index = avg
    | List.length avg < range - 1 = getMovingAvg input range index (avg List.++ [0])
    | otherwise = getMovingAvg input range (index + 1) (avg List.++ [sum (List.take (range) (List.drop index input)) / fromIntegral range])

movingAverage :: (Fractional a) => [a] -> Int -> [a]
movingAverage xs range = getMovingAvg xs range 0 []

-- ghci> movingAverage [1..10] 4
-- [0.0,0.0,0.0,2.5,3.5,4.5,5.5,6.5,7.5,8.5]
-- ghci> movingAverage [1..10] 5
-- [0.0,0.0,0.0,0.0,3.0,4.0,5.0,6.0,7.0,8.0]
-- ghci> movingAverage [0..10] 4
-- [0.0,0.0,0.0,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5]

