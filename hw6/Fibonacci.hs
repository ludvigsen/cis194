{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

-- 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..] 


-- 2
fibs2 :: [Integer]
fibs2 = fibs2' 0  1 []
    where fibs2' n n2 fibs = [n] ++ fibs2' n2 (n+n2) fibs


-- 3
data Stream a = Cons a (Stream a)

instance (Show a) => Show (Stream a) where
    show s = show $ take 20 $ streamToList s

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x : streamToList y

-- 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))


-- 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a s) (Cons a2 s2) = Cons a ((Cons a2) (interleaveStreams s s2))


-- 1/2^0, 2/2^1, 3/2^0, 4/2^2, 5/2^0, 6/2^1, 7/2^0, 8/2^3, 9/2^0, 10/2^1, 11/2^0, 12/2^2, 13/2^0, 14/2^1, 15/2^0, 16/2^4
-- This should be improved...
ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (biggestDivisablePow2 0) (streamFromSeed (+2) 2))
    where biggestDivisablePow2 n x | (x `mod` (2^n)) == 0 = biggestDivisablePow2 (n+1) x
                                   | otherwise = (n-1)

-- 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = (Cons n (streamRepeat 0))
    negate s = streamMap (\x -> -x) s
    (+) (Cons a1 s1) (Cons a2 s2) = Cons (a1+a2) ((+) s1 s2)
    (*) (Cons a1 s1) b@(Cons a2 s2) = Cons (a1*a2) ((streamMap (*a1) s2) + (s1 * b))


instance Fractional (Stream Integer) where
    (/) a@(Cons a1 s1) b@(Cons a2 s2) = Cons (a1 `div` a2) (streamMap (*(1 `div` a2)) (s1 - (a / b)*s2))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- 7
data Matrix = Matrix Integer Integer Integer Integer
    deriving (Show)

instance Num Matrix where
    (*) (Matrix m1x1 m1x2 m1y1 m1y2) (Matrix m2x1 m2x2 m2y1 m2y2) = 
        Matrix ((m1x1 * m2x1) + (m1x2 * m2y1))
               ((m1x1 * m2x2) + (m1x2 * m2y2))
               ((m1y1 * m2x1) + (m1y2 * m2y1))
               ((m1y1 * m2x2) + (m1y2 * m2y2))

fib4 :: Integer -> Integer
fib4 n = getAnswer ((Matrix 1 1 1 0)^n)
    where getAnswer (Matrix _ x _ _) = x
