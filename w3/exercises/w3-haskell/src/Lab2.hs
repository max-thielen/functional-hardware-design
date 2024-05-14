{-# OPTIONS_GHC -Wno-orphans #-}

module Lab2 where

import Clash.Prelude

-- ex12:
fourthBit :: BitVector 8 -> Bool
fourthBit vec = testBit vec 3
-- ghci> fourthBit 0b00000000
-- False
-- ghci> fourthBit 0b00001000
-- True

-- ex13:
myPack :: Maybe(Unsigned 8) -> BitVector 8
myPack Nothing = 0
myPack (Just x) = pack x
-- ghci> myPack Nothing
-- 0b0000_0000
-- ghci> myPack (Just 255)
-- 0b1111_1111
-- ghci> myPack (Just 0b00101001)
-- 0b0010_1001

-- ex14:
absSigned :: Signed 8 -> Unsigned 8
absSigned x = fromIntegral $ abs x
-- ghci> absSigned 0b11001011
-- 53
-- ghci> absSigned 0b00110101
-- 53

-- ex15:
absSignedPoly :: KnownNat n => Signed n -> Unsigned n
absSignedPoly x = fromIntegral $ abs x
-- ghci> absSignedPoly (0b11001011 :: Signed 8)
-- 53
-- ghci> absSignedPoly (-1 :: Signed 16)
-- 1

-- ex16:
safeAddUnsigned :: KnownNat n => Unsigned n -> Unsigned n -> Unsigned (n+1)
safeAddUnsigned x y = resize x + resize y
-- also: resize (x + y) whats the difference? (cause this does not work with safeMulUnsigned)

-- ghci> safeAddUnsigned (1 :: Unsigned 8) (1 :: Unsigned 8)  
-- 2
-- ghci> safeAddUnsigned (0b00010001 :: Unsigned 8) (1 :: Unsigned 8)
-- 18

-- ex17:
safeAddIndex :: (KnownNat n, KnownNat m) => Index n -> Index m -> Index (n + m)
safeAddIndex x y = resize x + resize y
-- ghci> safeAddIndex (3::Index 4) (3::Index 4)
-- 6
-- ghci> :t it
-- it :: Index 8

-- ex18:
safeSubUnsigned :: (KnownNat n, KnownNat m) => Unsigned n -> Unsigned m -> Signed ((Max n m)+1)
safeSubUnsigned x y = (bitCoerce $ resize (x)) - (bitCoerce $ resize (y))
-- ghci> safeSubUnsigned (0b11111111 :: Unsigned 8) (0b00000001 :: Unsigned 8)
-- 254
-- ghci> :t it
-- it :: Unsigned 8
-- ghci> safeSubUnsigned (minBound::Unsigned 2) (maxBound::Unsigned 64)
-- -18446744073709551615

safeMulUnsigned :: KnownNat n => Unsigned n -> Unsigned n -> Unsigned (n * 2)
safeMulUnsigned x y = resize x * resize y
-- ghci> safeMulUnsigned (0b11111111 :: Unsigned 8) (0b11111111 :: Unsigned 8)
-- 65025
-- ghci> :t it
-- it :: Unsigned 16

-- ex19:
-- ghci> foo = 0 :> 1 :> 2 :> Nil
-- ghci> :t foo
-- foo :: Num a => Vec 3 a

-- ghci> bar v = minBound :> maxBound :> Nil ++ v
-- ghci> :t bar
-- bar :: Bounded a => Vec n a -> Vec ((n + 1) + 1) a

-- ghci> mySnat = d3
-- ghci> :t mySnat
-- mySnat :: SNat 3

-- ex20:
type ThreeDVector = (Double, Double, Double)

testType :: ThreeDVector -> ThreeDVector
testType x = x
-- ghci> :t testType  
-- testType :: ThreeDVector -> ThreeDVector
-- ghci> testType (2,2,2)
-- (2.0,2.0,2.0)

-- ex21:
type TwoDVector = (Double, Double)

vectorMagnitude2D :: TwoDVector -> Double
vectorMagnitude2D (x, y) = sqrt ((x ** 2) + (y ** 2))

vectorMagnitude3D :: ThreeDVector -> Double
vectorMagnitude3D (x, y, z) = sqrt ((x ** 2) + (y ** 2) + (z ** 2))

convertTo3D :: TwoDVector -> ThreeDVector
convertTo3D (x, y) = (x, y, 0)

-- ghci> convertTo3D (2,3)
-- (2.0,3.0,0.0)
-- still works :)

-- ex22:
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Show, Eq, Enum)

-- ex23:
type Height = Double
type Width = Double
type Radius = Double
-- type Rectangle = (Height, Width)
-- type Circle = Radius

data Shape = Rectangle (Height, Width) | Circle Radius
    deriving (Show, Eq)

-- ex24:
-- ghci> Monday == Monday
-- True
-- ghci> Monday == Tuesday
-- False
-- ghci> Monday /= Sunday
-- True
-- ghci> Rectangle (2,2) == Rectangle (2,2)
-- True
-- ghci> Circle 3 == Circle 3
-- True

-- ex25:
nextDay :: Weekday -> Weekday
nextDay Sunday = Monday
nextDay x = succ x

-- ghci> nextDay Monday
-- Tuesday
-- ghci> nextDay Sunday
-- Monday

-- ex26:
getArea :: Shape -> Double
getArea (Circle r) = 3.14 * r * r
getArea (Rectangle (h, w)) = h * w 

-- ghci> getArea (Circle 1)   
-- 3.14
-- ghci> getArea (Circle 2)
-- 12.56
-- ghci> getArea (Rectangle (2,2))
-- 4.0

-- ex27: 
getPerim :: Shape -> Double
getPerim (Circle r) = 3.14 * r * 2
getPerim (Rectangle (h, w)) = (h + w) * 2

-- ghci> getPerim (Circle 1)
-- 6.28
-- ghci> getPerim (Circle 3)
-- 18.84
-- ghci> getPerim (Rectangle (3,3))
-- 12.0

-- ex28:
type Status = Bool
data Protocol = TCP | UTP
type Port = Integer
type IP = String

type NetworkConnection = (IP, Port, Protocol, Status)

-- ex29:
type MAC = String
type SubnetMask = String

type NetworkInterface = (MAC, IP, SubnetMask, Status)

-- ex30:
instance Ord Shape where
    compare x y = compare (getArea x) (getArea y)

-- ghci> Circle 1 < Circle 2
-- True
-- ghci> Circle 2 < Circle 2 
-- False
-- ghci> Circle 2 <= Circle 2
-- True

-- ex31:
midPoint :: forall n . (Bounded n, Integral n) => n
midPoint = fromIntegral $ (toInteger (minBound :: n) + toInteger (maxBound :: n)) `div` 2

-- ex32:
