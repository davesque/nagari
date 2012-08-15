module ParseFern where

import Prelude hiding (return, iterate)
import Data.Char
import Data.Map (Map, fromList, insert)

import ParseCore

-------------------
-- Grammar types --
-------------------

-- | Language expression type.
data Expr = Num Int
          | Var String
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Show)

-- | Language statement type.
data Statement = Assignment Expr Expr
               | Print Expr
               deriving (Show)

-- | Map to store variable names in scope.
type Scope    = Map String Expr

---------------------
-- Grammar parsers --
---------------------

-- | Parses a word as a token and returns it as a `Var` value.
var :: Parser Expr
var = word >>> Var

-- | Parses a number as a token and returns it as a `Num` value.
num :: Parser Expr
num = number >>> Num

-- | Parses a multiplication or division operator and returns a Mul or Div
-- value.
mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = token $
    lit '*' >>> (\_ -> Mul)
  ! lit '/' >>> (\_ -> Div)

-- | Parses an addition or subtraction operator and returns a Add or Sub value.
addOp :: Parser (Expr -> Expr -> Expr)
addOp = token $
    lit '+' >>> (\_ -> Add)
  ! lit '-' >>> (\_ -> Sub)

-- | Parses a value and returns an Expr type.
value :: Parser Expr
value = num ! var ! expr
      ! err "Illegal value"

-- | Parses a whole expression and returns an Expr type.
expr :: Parser Expr
expr = token (lit '(') -# token addExpr #- token (lit ')')

-- | Builds an operator Expr value.
bldOp :: Expr -> (Expr -> Expr -> Expr, Expr) -> Expr
bldOp e (o, e') = o e e'

-- | Recursive multiplication expression builder.
mulExpr' :: Expr -> Parser Expr
mulExpr' e = ((mulOp # value) >>> bldOp e) >>- mulExpr'
           ! return e

-- | Parses a multiplication expression and returns an Expr value.
mulExpr :: Parser Expr
mulExpr = value >>- mulExpr'

-- | Recursive addition expression buider.
addExpr' :: Expr -> Parser Expr
addExpr' e = ((addOp # mulExpr) >>> bldOp e) >>- addExpr'
           ! return e

-- | Parses an addition expression and returns an Expr value.
addExpr :: Parser Expr
addExpr = mulExpr >>- addExpr'

-- | Requires a string s to be parsed as a token or an error is thrown.
require :: String -> Parser String
require s = token (iterate char (length s) ? (==s))
          ! err ("Required string '" ++ s ++ "' not found")

-- | Builds a statement value from a tuple.
bldStatement :: (Expr, Expr) -> Statement
bldStatement (x, y) = Assignment x y

-- | Parses a statement and returns a Statement value.
statement :: Parser Statement
statement = (var #- becomes) # (addExpr ! value) >>> bldStatement
