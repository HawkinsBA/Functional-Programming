{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module AParser (Parser, runParser, satisfy, char, posInt) where

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a where
  P :: (String -> Maybe (a, String)) -> Parser a

runParser :: Parser a -> String -> Maybe (a, String)
runParser (P f) = f

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P f
  where
    f [] = Nothing    -- fail on an empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, I've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = P f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap g (P t1) = P (\s -> case t1 s of
    Nothing -> Nothing
    Just (a, s2) -> Just (g a, s2))