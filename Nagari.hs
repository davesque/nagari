module Nagari where

import Prelude hiding (return, fail, iterate)
import Data.Char
import Data.List hiding (iterate)

----------------
-- Data types --
----------------

-- | Parser result type.
type ParserResult a = Maybe (a, String)

-- | Parser (combinator) type.
type Parser a = String -> ParserResult a

-----------------------
-- Utility functions --
-----------------------

-- | Returns a tuple of its two arguments.
build :: a -> b -> (a, b)
build x y = (x, y)

-- | Applies the cons operator to the members of a double.
cons :: (a, [a]) -> [a]
cons (x, xs) = x:xs

-- | Combines a parser result containing an element and a parser result
-- containing a list of elements to construct a parser result with a new list.
-- Used only in iterateWhile.
cons' :: ParserResult a -> ParserResult [a] -> ParserResult [a]
cons' Nothing _ = Nothing
cons' _ Nothing = Nothing
cons' (Just (a, _)) (Just (as, s2)) = Just (a:as, s2)

-- | Prints error message.
err :: String -> Parser a
err m cs = error $ m ++ " near '" ++ cs ++ "'\n"

---------------
-- Operators --
---------------

infix  4 ?    -- pfilter
infixl 3 #    -- pcat
infixl 3 #-   -- pfst
infixl 3 -#   -- psnd
infixl 2 >>>  -- pmap
infix  2 >>-  -- pbind
infixl 1 !    -- palternative

-- | Filters the result of a parser `p` with a boolean function `f`.
pfilter :: (a -> Bool) -> Parser a -> Parser a
pfilter f p xs = case p xs of
    Nothing -> Nothing
    r@(Just (y, _)) -> if f y then r else Nothing
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

-- | Returns the result of the first of two parsers `p` and `q`.
pfst :: Parser a -> Parser b -> Parser a
pfst p q = (p # q) >>> fst
(#-) = pfst

-- | Returns the result of the second of two parsers `p` and `q`.
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
fail _ = Nothing

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

-- | Parses a single char equal to `x`.
lit :: Char -> Parser Char
lit x = char ? (==x)

-- | Parses a single char not equal to `x`.
unlit :: Char -> Parser Char
unlit x = char ? (/=x)

-- | Parses two of the same char.
double :: Parser Char
double = char >>- lit

---------------------
-- Complex parsers --
---------------------

-- | Applies a parser to a string `i` times and concatenates the results into
-- an array.
iterate :: Parser a -> Int -> Parser [a]
iterate _ 0 = return []
iterate p i = p # iterate p (i-1) >>> cons

-- | Parses a string while a parser `p` succeeds and returns all results as an
-- array.  Extremely slow for some reason.
iterateWhile' :: Parser a -> Parser [a]
iterateWhile' p = p # iterateWhile' p >>> cons
                ! return []

-- | A faster, less elegant implementation of iterateWhile.
iterateWhile :: Parser a -> Parser [a]
iterateWhile p xs = case p xs of
    Nothing      -> Just ([], xs)
    Just (y, ys) -> cons' (Just (y, ys)) (iterateWhile p ys)

-- | Finds the index of the first occurrence of a list `a` in a list `b`.
findIn :: (Eq a) => [a] -> [a] -> Maybe Int
findIn _ [] = Nothing
findIn [] _ = Nothing
findIn a b  = elemIndex True $ map (isPrefixOf a) (tails b)

-- | Parses a string until an occurrence of string `a` is found.  If no
-- occurrence is found, returns Nothing.
iterateUntil :: String -> Parser String
iterateUntil a b = case findIn a b of
    Nothing -> Nothing
    Just x  -> Just (splitAt x b)

-- | Converts a parser `p` into a parser which will clear any whitespace after
-- the successfully parsed portion of a string.
token :: Parser a -> Parser a
token p = p #- iterateWhile space

-- | Parses an assignment operator ('=').
becomes :: Parser Char
becomes = token $ lit '='

-- | Parses a sequence of letters.
letters :: Parser String
letters = letter # iterateWhile letter >>> cons

-- | Parses a word as a token.
word :: Parser String
word = token letters

-- | Parses a number and returns its integer value.
number :: Parser Int
number = iterateWhile digit >>- \x -> case x of
    [] -> fail
    _  -> return $ read x

-- | Parses a string equal to `s`.
accept :: String -> Parser String
accept s = iterate char (length s) ? (==s)

-----------------
-- Old parsers --
-----------------

{--- | Parses two chars.-}
{-twochars :: Parser (Char, Char)-}
{-twochars = char # char-}

{--- | Returns the second char of two parsed chars.-}
{-sndChar :: Parser Char-}
{-sndChar = twochars >>> snd-}

{--- | Returns two parsed chars as a string.-}
{-twochars' :: Parser String-}
{-twochars' = char # char >>> (\(x, y) -> [x, y])-}

{--- | Parses a semicolon.-}
{-semicolon :: Parser Char-}
{-semicolon = lit ';'-}

{--- | Parses a single digit char and converts it to an integer value.-}
{-digitVal :: Parser Int-}
{-digitVal = digit >>> digitToInt-}

{--- | Parses a single alphabetical char and converts it to uppercase.-}
{-upcaseLetter :: Parser Char-}
{-upcaseLetter = letter >>> toUpper-}

{--- | Parses two chars of the same value.  The `char` parser parses one char,-}
{--- which is then passed to `lit` to create another parser which will accept the-}
{--- same char.-}
{-double :: Parser Char-}
{-double = char >>- lit-}

{--- | Requires a string s to be parsed as a token or an error is thrown.-}
{-require :: String -> Parser String-}
{-require s = token (iterate char (length s) ? (==s))-}
          {-! err ("Required string '" ++ s ++ "' not found")-}