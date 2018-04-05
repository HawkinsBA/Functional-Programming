{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import ExprT
import Parser
import StackVM

-- Exercise 1

eval :: ExprT -> Integer
eval = foldExprT (id) (+) (*)

foldExprT :: (Integer -> a) -> (a -> a -> a) -> (a -> a -> a) -> ExprT -> a
foldExprT f _ _ (Lit x) = f x
foldExprT f g h (Add x y) = g (foldExprT f g h x) (foldExprT f g h y)
foldExprT f g h (Mul x y) = h (foldExprT f g h x) (foldExprT f g h y)

-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr expr = eval <$> (parseExp Lit Add Mul expr)

-- Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit :: Integer -> ExprT
  lit x = Lit x
  add :: ExprT -> ExprT -> ExprT
  add x y = Add x y
  mul :: ExprT -> ExprT -> ExprT
  mul x y = Mul x y

-- Exercise 4

instance Expr Integer where
  lit :: Integer -> Integer
  lit x = id x
  add :: Integer -> Integer -> Integer
  add x y = x + y
  mul :: Integer -> Integer -> Integer
  mul x y = x * y

instance Expr Bool where
  lit :: Integer -> Bool
  lit x = x > 0
  add :: Bool -> Bool -> Bool
  add x y = x || y
  mul :: Bool -> Bool -> Bool
  mul x y = x && y

newtype MinMax = MinMax Integer deriving (Ord, Eq, Show)

instance Expr MinMax where
  lit :: Integer -> MinMax
  lit x = MinMax x
  add :: MinMax -> MinMax -> MinMax
  add x y = max x y
  mul :: MinMax -> MinMax -> MinMax
  mul x y = min x y

newtype Mod7 = Mod7 Integer deriving (Num, Ord, Eq, Show)

instance Expr Mod7 where
  lit :: Integer -> Mod7
  lit x
    | (x < 7) && (x >= 0) = Mod7 x
    | otherwise = Mod7 (mod x 7)
  add :: Mod7 -> Mod7 -> Mod7
  add (Mod7 x) (Mod7 y) = Mod7 (mod (x + y) 7)
  mul :: Mod7 -> Mod7 -> Mod7
  mul (Mod7 x) (Mod7 y) = Mod7 (mod (x * y) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
