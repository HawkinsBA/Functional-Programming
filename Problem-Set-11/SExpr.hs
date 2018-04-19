{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

module SExpr where

import           AParser
import           Data.Char
import           Control.Applicative

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom where
  N :: Integer -> Atom
  I :: Ident   -> Atom
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr where
  A    :: Atom    -> SExpr
  Comb :: [SExpr] -> SExpr
  deriving Show

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 6

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

-- Exercise 7

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (++) <$> oneOrMore (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

-- Exercise 8

parseAtom :: Parser Atom
parseAtom = N <$> posInt <|> I <$> ident

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (A <$> parseAtom <|> char '(' <*> oneOrMore parseSExpr <*> char ')') <* spaces
