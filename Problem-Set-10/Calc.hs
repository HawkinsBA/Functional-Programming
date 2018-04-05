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
  add x y = x + y
  mul :: ExprT -> ExprT -> ExprT
  mul x y = x * y
