-- ex1:
    -- True :: Bool
    -- 5 :: Num a => a
    -- 5.0 :: Fractional a => a
    -- (==) :: Eq a => a -> a -> Bool
    -- (+) :: Num a => a -> a -> a
    -- (/) :: Fractional a => a -> a -> a
    -- mod :: Integral a => a -> a -> a
    -- fromIntegral :: (Integral a, Num b) => a -> b
    -- fromInteger :: Num a => Integer -> a
    -- Just True :: Maybe Bool
    -- Nothing :: Maybe a
    -- Left 5 :: Num a => Either a b

-- ex2:
    -- (+) :: Num a => a -> a -> a
    -- (1 +) :: Num a => a -> a
    -- (1 + 2) :: Num a => a

-- ex3:
    -- (+ 1) :: Num a => a -> a
    -- ((1 :: Int) +) :: ((1 :: Int) +) :: Int -> Int

    --  ( (1 :: Int) + (2 :: Integer) )
        -- Couldn't match expected type `Int' with actual type `Integer'

-- ex4:
myJustInt :: Maybe Int -> Bool
myJustInt x = if x == Nothing then False else True
-- ghci> myJustInt (Just 3)
-- True
-- ghci> myJustInt Nothing
-- False

-- ex5:
getLeft :: Either a b -> Maybe a
getLeft (Left x) = Just x
getLeft _ = Nothing

-- ex6:
isNegative :: (Ord a, Num a) => a -> Bool
isNegative x = x < 0

-- ex7:
isZero :: (Eq a, Num a) => a -> Bool
isZero x = x == 0

-- ex8:
-- ghci> minBound @Int
-- -9223372036854775808
-- ghci> maxBound @Int
-- 9223372036854775807

-- ghci> minBound @Bool
-- False
-- ghci> maxBound @Bool
-- True

-- ghci> minBound @Char
-- '\NUL'
-- ghci> maxBound @Char
-- '\1114111'

-- ex9:
getFractional :: (Show a, Fractional a) => a -> Int
getFractional n = read xs :: Int
    where
        str = show n
        (x:y:xs) = str
-- ghci> getFractional 1
-- 0
-- ghci> getFractional 2.654
-- 654

-- ex10:
nextChar :: Char -> Char
nextChar c = toEnum((fromEnum c) + 1);
-- ghci> nextChar 'c' 
-- 'd'
-- ghci> nextChar '\DEL'
-- '\128'

-- ex11:
nextLetter :: Char -> Char
nextLetter x 
    | 'a' <= x && x < 'z' = nextChar x
    | x == 'z' = 'a'
    | 'A' <= x && x < 'Z' = nextChar x
    | x == 'Z' = 'A'
    | otherwise = x
-- ghci> nextLetter '('
-- '('
-- ghci> nextLetter 'a'
-- 'b'
-- ghci> nextLetter 'z'
-- 'a'
-- ghci> nextLetter 'Z'
-- 'A'
-- ghci> nextLetter '\DEL'
-- '\DEL'