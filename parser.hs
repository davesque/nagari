module Parse where

import Prelude hiding (fail, return, iterate)
import Data.Char

-- | Returns a tuple of its two arguments.
build :: a -> b -> (a, b)
build x y = (x, y)

-- | Applies the cons operator to the members of a double.
cons :: (a, [a]) -> [a]
cons (x, xs) = x:xs

-- | Language expression type.
data Expr = Num Int
          | Var String
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Show)

-- | Language statement type.
data Statement = Assignment String Expr
               deriving (Show)

-- | Parser (combinator) type.
type Parser a = String -> Maybe (a, String)

-- | Prints error message.
err :: String -> Parser a
err m cs = error $ m ++ " near '" ++ cs ++ "'\n"

-- | Filters the result of a parser `p` with a boolean function `f`.
pfilter :: (a -> Bool) -> Parser a -> Parser a
pfilter f p xs = case p xs of
    Nothing -> Nothing
    r@(Just (y, ys)) -> if f y then r else Nothing
infix 7 ?
p ? f = pfilter f p

-- | Returns the result of an alternative parser `q` if a parser `p` fails.
palternative :: Parser a -> Parser a -> Parser a
palternative p q xs = case p xs of
    Nothing -> q xs
    _       -> p xs
infixl 3 !
(!) = palternative

-- | Maps a function `f` over the parsed portion of the result of a parser `p`.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p xs = case p xs of
    Nothing      -> Nothing
    Just (y, ys) -> Just (f y, ys)
infixl 5 >>-
p >>- f = pmap f p

-- | Provides the result of a parser `p` to another parser which is returned by
-- a function `f`.
pbind :: (a -> Parser b) -> Parser a -> Parser b
pbind f p xs = case p xs of
    Nothing      -> Nothing
    Just (y, ys) -> f y ys
infix 4 #>
p #> f = pbind f p

-- | Concatenates the results of two parsers `p` and `q` into a tuple.
pcat :: Parser a -> Parser b -> Parser (a, b)
pcat p q = p #> (\x -> q >>- build x)
infixl 6 #
(#) = pcat

-- | Returns the result of the first of two parsers `p` and `q`.
psnd :: Parser a -> Parser b -> Parser b
psnd p q = (p # q) >>- snd
infixl 4 -#
(-#) = psnd

-- | Returns the result of the second of two parsers `p` and `q`.
pfst :: Parser a -> Parser b -> Parser a
pfst p q = (p # q) >>- fst
infixl 4 #-
(#-) = pfst

-- | Always succeeds in parsing a value `x`.
return :: a -> Parser a
return x xs = Just (x, xs)

-- | Always fails to parse.
fail :: Parser a
fail xs = Nothing

{-
 - Applies a parser to a string `i` times and concatenates the results into an
 - array.
 -}
iterate :: Parser a -> Int -> Parser [a]
iterate p 0 = return []
iterate p i = p # iterate p (i-1) >>- cons

{-
 - Parses a string while a parser `p` succeeds and returns all results as an
 - array.
 -}
iterateWhile :: Parser a -> Parser [a]
iterateWhile p = p # iterateWhile p >>- cons ! return []

{-
 - Converts a parser `p` into a parser which will clear any whitespace after
 - the successfully parsed portion of a string.
 -}
token :: Parser a -> Parser a
token p = p #- (iterateWhile space)

{-
 - Parses a single char.
 -}
char :: Parser Char
char "" = Nothing
char (x:xs) = Just (x, xs)

{-
 - Parses a single char that is a digit.
 -}
digit :: Parser Char
digit = char ? isDigit

{-
 - Parses a single char that is whitespace.
 -}
space :: Parser Char
space = char ? isSpace

{-
 - Parses a single char that is alphabetical.
 -}
letter :: Parser Char
letter = char ? isAlpha

{-
 - Parses a single char that is alphanumeric.
 -}
alphanum :: Parser Char
alphanum = letter ! digit

{-
 - Parses a single char `x`.
 -}
lit :: Char -> Parser Char
lit x = char ? (==x)

{-
 - Parses two chars.
 -}
twochars :: Parser (Char, Char)
twochars = char # char

{-
 - Parses a semicolon.
 -}
semicolon :: Parser Char
semicolon = lit ';'

{-
 - Parses an assignment operator (':=').
 -}
becomes :: Parser (Char, Char)
becomes = twochars ? (==(':', '='))

{-
 - Parses a single digit char and converts it to an integer value.
 -}
digitVal :: Parser Int
digitVal = digit >>- digitToInt

{-
 - Parses a single alphabetical char and converts it to uppercase.
 -}
upcaseLetter :: Parser Char
upcaseLetter = letter >>- toUpper

{-
 - Returns the second char of two parsed chars.
 -}
sndChar :: Parser Char
sndChar = twochars >>- snd

{-
 - Returns two parsed chars as a string.
 -}
twochars' :: Parser String
twochars' = (char # char) >>- (\(x, y) -> [x, y])

{-
 - Parses a sequence of letters.
 -}
letters :: Parser String
letters = letter # iterateWhile letter >>- cons

{-
 - Parses a word as a token.
 -}
word :: Parser String
word = token letters

{-
 - Parses a string s as a token.
 -}
accept :: String -> Parser String
accept s = token (iterate char (length s) ? (==s))

{- 
 - Parses two of the same char value.  The `char` parser parses one char, which
 - is then passed to `lit` to create another parser which will accept the same
 - char.
 -}
double :: Parser Char
double = char #> lit

{-
 - Parses a number as a token and returns its integer value.
 -}
number :: Parser Int
number = token (iterateWhile digit) #> \x -> case x of
    [] -> fail
    _  -> return $ read x

{-
 - Parses a word as a token and returns it as a `Var` value.
 -}
var :: Parser Expr
var = word >>- Var

{-
 - Parses a number as a token and returns it as a `Num` value.
 -}
num :: Parser Expr
num = number >>- Num

{-
 - Parses a multiplication or division operator and returns a Mul or Div value.
 -}
mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = lit '*' >>- (\_ -> Mul)
      ! lit '/' >>- (\_ -> Div)

{-
 - Parses an addition or subtraction operator and returns a Add or Sub value.
 -}
addOp :: Parser (Expr -> Expr -> Expr)
addOp = lit '+' >>- (\_ -> Add)
      ! lit '-' >>- (\_ -> Sub)

{-
 - Parses a 'factor' and returns an Expr type.
 -}
factor :: Parser Expr
factor = num ! var ! lit '(' -# expr #- lit ')'
       ! err "Illegal factor"

{-
 - Builds an op Expr value.
 -}
bldOp :: Expr -> (Expr -> Expr -> Expr, Expr) -> Expr
bldOp e (o, e') = o e e'

{-
 - Recursive term buider.
 -}
term' :: Expr -> Parser Expr
term' e = (((mulOp # factor) >>- bldOp e) #> term')
        ! return e

{-
 - Parses a term and returns an Expr value.
 -}
term :: Parser Expr
term = factor #> term'

{-
 - Recursive expr buider.
 -}
expr' :: Expr -> Parser Expr
expr' e = (((addOp # term) >>- bldOp e) #> expr')
        ! return e

{-
 - Parses an expr and returns an Expr value.
 -}
expr :: Parser Expr
expr = term #> expr'

{-
 - Requires a string s to be parsed as a token or an error is thrown.
 -}
require :: String -> Parser String
require s = token (iterate char (length s) ? (==s))
          ! err ("Required string '" ++ s ++ "' not found")
