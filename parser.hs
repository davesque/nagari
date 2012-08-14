module Parse where

import Prelude hiding (fail, iterate, lookup)
import Data.Char
import Data.Map (Map, lookup, fromList, insert)


----------------
-- Data types --
----------------

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
               deriving (Show)

-- | Parser (combinator) type.
type Parser a = String -> Maybe (a, String)
type Scope    = Map String Expr


-----------------------
-- Utility functions --
-----------------------

-- | Returns a tuple of its two arguments.
build :: a -> b -> (a, b)
build x y = (x, y)

-- | Applies the cons operator to the members of a double.
cons :: (a, [a]) -> [a]
cons (x, xs) = x:xs

-- | Prints error message.
err :: String -> Parser a
err m cs = error $ m ++ " near '" ++ cs ++ "'\n"


---------------
-- Operators --
---------------

infix  5 ?    -- pfilter
infixl 4 #    -- pcat
infixl 3 >>>  -- pmap
infix  3 >>-  -- pbind
infixl 2 #-   -- pfst
infixl 2 -#   -- psnd
infixl 1 !    -- palternative

-- | Filters the result of a parser `p` with a boolean function `f`.
pfilter :: (a -> Bool) -> Parser a -> Parser a
pfilter f p xs = case p xs of
    Nothing -> Nothing
    r@(Just (y, ys)) -> if f y then r else Nothing
p ? f = pfilter f p

-- | Returns the result of an alternative parser `q` if a parser `p` fails.
palternative :: Parser a -> Parser a -> Parser a
palternative p q xs = case p xs of
    Nothing -> q xs
    _       -> p xs
(!) = palternative

-- | Maps a function `f` over the parsed portion of the result of a parser `p`.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p xs = case p xs of
    Nothing      -> Nothing
    Just (y, ys) -> Just (f y, ys)
p >>> f = pmap f p

-- | Provides the result of a parser `p` to another parser which is returned by
-- a function `f`.
pbind :: (a -> Parser b) -> Parser a -> Parser b
pbind f p xs = case p xs of
    Nothing      -> Nothing
    Just (y, ys) -> f y ys
p >>- f = pbind f p

-- | Concatenates the results of two parsers `p` and `q` into a tuple.
pcat :: Parser a -> Parser b -> Parser (a, b)
pcat p q = p >>- (\x -> q >>> build x)
(#) = pcat

-- | Returns the result of the second of two parsers `p` and `q`.
pfst :: Parser a -> Parser b -> Parser a
pfst p q = (p # q) >>> fst
(#-) = pfst

-- | Returns the result of the first of two parsers `p` and `q`.
psnd :: Parser a -> Parser b -> Parser b
psnd p q = (p # q) >>> snd
(-#) = psnd


--------------------
-- Core functions --
--------------------

-- | Always succeeds in parsing a value `x`.
return' :: a -> Parser a
return' x xs = Just (x, xs)

-- | Always fails to parse.
fail :: Parser a
fail xs = Nothing

-- | Applies a parser to a string `i` times and concatenates the results into
-- an array.
iterate :: Parser a -> Int -> Parser [a]
iterate p 0 = return' []
iterate p i = p # iterate p (i-1) >>> cons

-- | Parses a string while a parser `p` succeeds and returns all results as an
-- array.
iterateWhile :: Parser a -> Parser [a]
iterateWhile p = p # iterateWhile p >>> cons
               ! return' []

-- | Converts a parser `p` into a parser which will clear any whitespace after
-- the successfully parsed portion of a string.
token :: Parser a -> Parser a
token p = p #- iterateWhile space


-------------
-- Parsers --
-------------

-- | Parses a single char.
char :: Parser Char
char "" = Nothing
char (x:xs) = Just (x, xs)

-- | Parses a single char that is a digit.
digit :: Parser Char
digit = char ? isDigit

-- | Parses a single char that is whitespace.
space :: Parser Char
space = char ? isSpace

-- | Parses a single char that is alphabetical.
letter :: Parser Char
letter = char ? isAlpha

-- | Parses a single char that is alphanumeric.
alphanum :: Parser Char
alphanum = letter ! digit

-- | Parses a single char `x`.
lit :: Char -> Parser Char
lit x = char ? (==x)

-- | Parses two chars.
twochars :: Parser (Char, Char)
twochars = char # char

-- | Parses a semicolon.
semicolon :: Parser Char
semicolon = lit ';'

-- | Parses an assignment operator (':=').
becomes :: Parser Char
becomes = token $ lit '='

-- | Parses a single digit char and converts it to an integer value.
digitVal :: Parser Int
digitVal = digit >>> digitToInt

-- | Parses a single alphabetical char and converts it to uppercase.
upcaseLetter :: Parser Char
upcaseLetter = letter >>> toUpper

-- | Returns the second char of two parsed chars.
sndChar :: Parser Char
sndChar = twochars >>> snd

-- | Returns two parsed chars as a string.
twochars' :: Parser String
twochars' = char # char >>> (\(x, y) -> [x, y])

-- | Parses a sequence of letters.
letters :: Parser String
letters = letter # iterateWhile letter >>> cons

-- | Parses a word as a token.
word :: Parser String
word = token letters

-- | Parses a string s as a token.
accept :: String -> Parser String
accept s = token $ iterate char (length s) ? (==s)

-- | Parses two chars of the same value.  The `char` parser parses one char,
-- which is then passed to `lit` to create another parser which will accept the
-- same char.
double :: Parser Char
double = char >>- lit

-- | Parses a number as a token and returns its integer value.
number :: Parser Int
number = token (iterateWhile digit) >>- \x -> case x of
    [] -> fail
    _  -> return' $ read x

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
           ! return' e

-- | Parses a multiplication expression and returns an Expr value.
mulExpr :: Parser Expr
mulExpr = value >>- mulExpr'

-- | Recursive addition expression buider.
addExpr' :: Expr -> Parser Expr
addExpr' e = ((addOp # mulExpr) >>> bldOp e) >>- addExpr'
           ! return' e

-- | Parses an addition expression and returns an Expr value.
addExpr :: Parser Expr
addExpr = mulExpr >>- addExpr'

-- | Requires a string s to be parsed as a token or an error is thrown.
require :: String -> Parser String
require s = token (iterate char (length s) ? (==s))
          ! err ("Required string '" ++ s ++ "' not found")

-- | Evaluate an Expr value.
eval :: Scope -> Maybe Expr -> Maybe Int
eval _ Nothing = Nothing
eval _ (Just (Num x)) = Just x
eval s (Just (Var x)) = case lookup x s of
    Nothing -> error $ "Variable '" ++ x ++ "' not in scope"
    e       -> eval s e
eval s (Just (Add x y)) = do
    a <- eval s (Just x)
    b <- eval s (Just y)
    return $ a + b
eval s (Just (Sub x y)) = do
    a <- eval s (Just x)
    b <- eval s (Just y)
    return $ a - b
eval s (Just (Mul x y)) = do
    a <- eval s (Just x)
    b <- eval s (Just y)
    return $ a * b
eval s (Just (Div x y)) = do
    a <- eval s (Just x)
    b <- eval s (Just y)
    return $ a `div` b

-- | Gets the parsed expression portion from an expression parser.
getExpr :: Maybe (Expr, String) -> Maybe Expr
getExpr Nothing = Nothing
getExpr (Just (x, xs)) = Just x

-- | Builds a statement value from a tuple.
bldStatement :: (Expr, Expr) -> Statement
bldStatement (x, y) = Assignment x y

-- | Parses a statement and returns a Statement value.
statement :: Parser Statement
statement = (var #- becomes) # (addExpr ! value) >>> bldStatement

-- | Executes a statement.
exec :: Scope -> Maybe Statement -> Scope
exec s Nothing = s
exec s (Just (Assignment (Var v) e)) = insert v e s

-- | Gets the parsed statement portion from a statement parser.
getStatement :: Maybe (Statement, String) -> Maybe Statement
getStatement Nothing = Nothing
getStatement (Just (x, xs)) = Just x

-- | Gets the expression portion from a statement.
getStatementExpr :: Maybe Statement -> Maybe Expr
getStatementExpr Nothing = Nothing
getStatementExpr (Just (Assignment v e)) = Just e

----------
-- Main --
----------

main = do
    contents <- readFile "test.calc"
    let strings    = lines contents
        statements = map (getStatement . statement) strings
        scope      = foldl (\a s -> exec a s) (fromList []) statements
        lastExpr   = getStatementExpr (last statements)
        final      = eval scope lastExpr
    putStrLn $ show ((\(Just x) -> x) final)
