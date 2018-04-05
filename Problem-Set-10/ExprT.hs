{-# LANGUAGE GADTs #-}
module ExprT where

data ExprT where
  Lit :: Integer -> ExprT
  Add :: ExprT -> ExprT -> ExprT
  Mul :: ExprT -> ExprT -> ExprT
  deriving (Show, Eq)
