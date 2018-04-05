{-# LANGUAGE GADTs #-}

module StackVM (StackVal(..), StackExp(..), Stack, Program, stackVM) where

-- Values that may appear in the stack. Such a value will also be
-- returned by the stackVM program execution function.
data StackVal where
  IVal :: Integer -> StackVal
  BVal :: Bool    -> StackVal
  Void ::            StackVal
  deriving Show

-- The various expressions our VM understands.
data StackExp where
  PushI :: Integer -> StackExp
  PushB :: Bool    -> StackExp
  AddOp ::            StackExp
  MulOp ::            StackExp
  AndOp ::            StackExp
  OrOp  ::            StackExp
  deriving Show

type Stack   = [StackVal]
type Program = [StackExp]

-- Execute the given program. Returns either an error message or the
-- value on top of the stack after execution.
stackVM :: Program -> Either String StackVal
stackVM = execute []

errType :: String -> Either String a
errType op = Left $ "Encountered '" ++ op ++ "' opcode with ill-typed stack."

errUnderflow :: String -> Either String a
errUnderflow op = Left $ "Stack underflow with '" ++ op ++ "' opcode."

-- Execute a program against a given stack.
execute :: Stack -> Program -> Either String StackVal
execute [] []                                 = Right Void
execute (s:_) []                              = Right s

execute s (PushI x : xs)                      = execute (IVal x : s) xs
execute s (PushB x : xs)                      = execute (BVal x : s) xs

execute (IVal s1 : IVal s2 : ss) (AddOp : xs) = execute (s':ss) xs
    where s' = IVal (s1 + s2)
execute (_:_:_) (AddOp:_)                     = errType "AddOp"
execute _ (AddOp:_)                           = errUnderflow "AddOp"

execute (IVal s1:IVal s2:ss) (MulOp : xs)     = execute (s':ss) xs
    where s' = IVal (s1 * s2)
execute (_:_:_) (MulOp:_)                     = errType "MulOp"
execute _ (MulOp:_)                           = errUnderflow "MulOp"

execute (BVal s1:BVal s2:ss) (AndOp : xs)     = execute (s':ss) xs
    where s' = BVal (s1 && s2)
execute (_:_:_) (AndOp:_)                     = errType "AndOp"
execute _ (AndOp:_)                           = errUnderflow "AndOp"

execute (BVal s1 : BVal s2 : ss) (OrOp : xs)  = execute (s':ss) xs
    where s' = BVal (s1 || s2)
execute (_:_:_) (OrOp:_)                      = errType "OrOp"
execute _ (OrOp:_)                            = errUnderflow "OrOp"

test = stackVM [PushI 3, PushI 5, AddOp]
