module ParseCore where

import Prelude hiding (return, fail, iterate)
import Data.Char

----------------
-- Data types --
----------------

-- | Parser (combinator) type.
type Parser a = String -> Maybe (a, String)

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

------------------
-- Core parsers --
------------------

-- | Always succeeds in parsing a value `x`.
return :: a -> Parser a
return x xs = Just (x, xs)

-- | Always fails to parse.
fail :: Parser a
fail xs = Nothing

-- | Parses a single char.
char :: Parser Char
char "" = Nothing
char (x:xs) = Just (x, xs)

-- | Parses a single digit char.
digit :: Parser Char
digit = char ? isDigit

-- | Parses a single whitespace char.
space :: Parser Char
space = char ? isSpace

-- | Parses a single alphabetical char.
letter :: Parser Char
letter = char ? isAlpha

-- | Parses a single alphanumeric char.
alphanum :: Parser Char
alphanum = letter ! digit

-- | Parses a single char `x`.
lit :: Char -> Parser Char
lit x = char ? (==x)

---------------------
-- Complex parsers --
---------------------

-- | Applies a parser to a string `i` times and concatenates the results into
-- an array.
iterate :: Parser a -> Int -> Parser [a]
iterate p 0 = return []
iterate p i = p # iterate p (i-1) >>> cons

-- | Parses a string while a parser `p` succeeds and returns all results as an
-- array.
iterateWhile :: Parser a -> Parser [a]
iterateWhile p = p # iterateWhile p >>> cons
               ! return []

-- | Converts a parser `p` into a parser which will clear any whitespace after
-- the successfully parsed portion of a string.
token :: Parser a -> Parser a
token p = p #- iterateWhile space

-- | Parses an assignment operator (':=').
becomes :: Parser Char
becomes = token $ lit '='

-- | Parses a sequence of letters.
letters :: Parser String
letters = letter # iterateWhile letter >>> cons

-- | Parses a word as a token.
word :: Parser String
word = token letters

-- | Parses a number as a token and returns its integer value.
number :: Parser Int
number = token (iterateWhile digit) >>- \x -> case x of
    [] -> fail
    _  -> return $ read x
