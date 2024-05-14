module AdventOfCode.Day1 where

import Prelude
import Data.Char ( digitToInt, isDigit )

charToInt :: Char -> Maybe Int
charToInt c
    | isDigit c = Just $ digitToInt c
    | otherwise = Nothing

-- returns list of all intergers in given string
-- due to recursion must give empty list
stringToIntList :: String -> [Int] -> [Int]
stringToIntList "" xs = xs
stringToIntList (c:string) xs =
  case charToInt c of
        Just n  -> stringToIntList string (xs ++ [n])
        Nothing -> stringToIntList string xs

getLastElement :: [a] -> a
getLastElement [] = error "Empty list"
getLastElement [x] = x
getLastElement (_:xs) = getLastElement xs

firstAndLast :: [a] -> [a]
firstAndLast [] = error "Empty list"
firstAndLast [x] = [x,x]
firstAndLast (x:xs) = [x,(getLastElement xs)]

-- takes list of two ints and combines it into one int
listToInt :: [Int] -> Int
listToInt [] = error "Empty List"
listToInt (x:y:_) = x*10 + y
listToInt [_] = error "I only take lists of length 2 :)"

parse :: [String] -> Int -> Int
parse [] count = count
parse (x:xs) count = parse xs (count + n)
  where
    n = listToInt $ firstAndLast $ stringToIntList x []

main :: IO ()
main = do
  content <- readFile "src/AdventOfCode/input/Day1.txt"
  let calibrationSum = parse (lines content) 0
  putStrLn "Sum of all calibration values is:"
  print calibrationSum
