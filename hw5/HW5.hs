{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}

import Parser
import StackVM
import Control.Exception
import Data.Typeable

class Expr a where
    mul :: a -> a -> a
    add :: a -> a -> a
    lit :: Integer -> a

data StackErrorException = StackErrorException
    deriving (Show, Typeable)

instance Exception StackErrorException

instance Expr Program where
    mul a b = case stackValA of 
                (Right (IVal valA)) -> case stackValB of 
                                        (Right (IVal valB)) -> [PushI valA, PushI valB, Mul]
                                        _ -> throw StackErrorException
                (Right (BVal valA)) -> case stackValB of
                                        (Right (BVal valB)) -> [PushB valA, PushB valB, Mul]
                                        _ -> throw StackErrorException
                _ -> throw StackErrorException
        where stackValA = stackVM a
              stackValB = stackVM b
    add a b = case stackValA of 
                (Right (IVal valA)) -> case stackValB of 
                                        (Right (IVal valB)) -> [PushI valA, PushI valB, Add]
                                        _ -> throw StackErrorException
                (Right (BVal valA)) -> case stackValB of
                                        (Right (BVal valB)) -> [PushB valA, PushB valB, Add]
                                        _ -> throw StackErrorException
                _ -> throw StackErrorException
        where stackValA = stackVM a
              stackValB = stackVM b
    lit a   = [PushI a]

compile :: String -> Maybe Program
compile = parseExp lit add mul

{-data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)-}

data MinMax = MinMax Integer
    deriving (Show, Eq)

data Mod7 = Mod8 Integer
    deriving (Show, Eq)

{-instance Expr ExprT where 
    mul a b = Mul a b
    add a b = Add a b
    lit a = Lit a-}

instance Expr MinMax where 
    mul (MinMax a) (MinMax b) | a>b = MinMax b
            | otherwise = MinMax a
    add (MinMax a) (MinMax b) | a>b = MinMax a
            | otherwise = MinMax b
    lit a = MinMax a

instance Expr Bool where
    mul a b = a && b
    add a b = a || b
    lit a = a >= 0

instance Expr Integer where
    mul a b = a * b
    add a b = a + b
    lit a = a

{-instance Expr Mod7 where
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7
    add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
    lit a = Mod7 $ a `mod` 7-}


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(4 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
--testSat = testExp :: Maybe Mod7
testProgram = testExp :: Maybe Program


--reify :: ExprT -> ExprT
--reify = id

reifyB :: Bool -> Bool
reifyB = id

--eval :: ExprT -> Integer
--eval (Lit a) = a
--eval (Mul expr1 expr2) = (eval expr1) * (eval expr2)
--eval (Add expr1 expr2) = (eval expr1) + (eval expr2)

--evalStr :: String -> Maybe Integer
--evalStr str = case expression of
    --(Just expr) -> Just $ eval expr
    --_ -> Nothing
  --where expression = parseExp Lit Add Mul str
