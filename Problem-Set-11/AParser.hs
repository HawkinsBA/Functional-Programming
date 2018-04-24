{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

module AParser (Parser, runParser, satisfy, char, posInt) where

import           Data.Char
import           Control.Applicative

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a where
  Parser :: (String -> Maybe (a, String)) -> Parser a

runParser :: Parser a -> String -> Maybe (a, String)
runParser (Parser f) = f

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
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
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap g (Parser f) = Parser (\s1 -> case f s1 of
    Nothing -> Nothing
    Just (a, s2) -> Just (g a, s2))

-- Exercise 2

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser (\s -> Just (a, s))
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1) <*> (Parser p2) = Parser (\s1 -> case p1 s1 of
    Nothing -> Nothing
    Just (f, s2) -> case p2 s2 of
      Nothing -> Nothing
      Just (b, s3) -> Just (f b, s3))

-- Exercise 3

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = pure () <$> abParser

intPair :: Parser [Integer]
intPair = (\n1 n2 -> [n1, n2]) <$> (posInt) <*> ((char ' ') *> posInt)

-- Exercise 4

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (\_ -> Nothing)
  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) = Parser (\s -> case p1 s of
    Just (f, s2) -> Just (f, s2)
    Nothing -> p2 s)

-- Exercise 5

intOrUppercase :: Parser ()
intOrUppercase = (\i -> ()) <$> posInt <|> (\i -> ()) <$> (satisfy isUpper)

quizParser :: Parser Integer
quizParser = char 'x' *> posInt
