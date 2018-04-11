module SExpr where

import           AParser

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

