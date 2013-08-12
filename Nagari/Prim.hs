module Nagari.Prim where

import Control.Monad
import Data.Char
import Data.Monoid
import Prelude hiding (take, takeWhile, map)
import qualified Prelude as P

import Nagari.Parser
import Nagari.State
import Nagari.Utils

------------------
-- Core parsers --
------------------

-- | Parses one character from the input.
char :: Parser Char
char = Parser $ \s -> case input s of
    []   -> (Nothing, s)
    x:xs -> let posModifier = if x == '\n' then incLine else incCol
            in (Just x, posModifier s { input = xs })

-- | Looks ahead one character in the input.
lookChar :: Parser Char
lookChar = Parser $ \s -> case input s of
    []  -> (Nothing, s)
    y:_ -> (Just y, s)

-- | Looks ahead `n` characters in the input.
look :: Int -> Parser String
look n = Parser $ \s -> case P.take n $ input s of
    []  -> (Nothing, s)
    inp -> (Just inp, s)

-- | Succeeds at parsing a single character if the given predicate is true for
-- the parser result.
charIf :: (Char -> Bool) -> Parser Char
charIf p = do
    x <- lookChar
    if p x then char else emptyOk

-- | Parses a single whitespace character.
space :: Parser Char
space = charIf isSpace

-- | Parses a single alphabetical character.
alpha :: Parser Char
alpha = charIf isAlpha

-- | Parses a single digit character.
digit :: Parser Char
digit = charIf isDigit

-- | Parses a single alpha-numerical character.
alphaNum :: Parser Char
alphaNum = charIf isAlphaNum

-- | Parses one of a given character `x`.
litChar :: Char -> Parser Char
litChar x = charIf (==x)

-- | Succeeds at parsing a character which is not the given character `x`.
unLitChar :: Char -> Parser Char
unLitChar x = charIf (/=x)

-- | Succeds at parsing any char in the given string.
accept :: String -> Parser Char
accept s = do
    x <- char
    if x `elem` s then return x else emptyOk

-- | Succeds at parsing any char which is not in the given string.
reject :: String -> Parser Char
reject s = do
    x <- char
    if x `notElem` s then return x else emptyOk

----------------------
-- Parsers builders --
----------------------

-- | Builds a parser that first attempts to parse with a parser `p` and falls
-- back to parsing with a parser `q` on failure.
or :: Parser a -> Parser a -> Parser a
or = mappend

{--- | Alias for `fmap`.-}
map :: (a -> b) -> Parser a -> Parser b
map = fmap

-- | Builds a parser which will apply itself to a string the given number of
-- times.
take :: Int -> Parser a -> Parser [a]
take = replicateM

{--- | Used as helper function by `takeAll`.-}
{-takeAll' :: Parser a -> Parser a-}
{-takeAll' p = Parser $ \ps ->-}
    {-let (x, ps') = runParser p ps-}
    {-in  [rs, runParser (takeAll' p) ps']-}

{--- | Builds a parser which will apply itself to a string until further-}
{--- applications yield no results.-}
{-takeAll :: Parser a -> Parser [a]-}
{-takeAll p = Parser $ \ps -> case runParser (takeAll' p) ps of-}
    {-[] -> []-}
    {-rs -> let unParsed = snd . last $ rs-}
              {-results  = P.map fst rs-}
          {-in [(results, unParsed)]-}

-- | Builds a parser that will succeed as long as the predicate `p` is true for
-- characters in the input.
takeWhile :: (Char -> Bool) -> Parser String
takeWhile p = Parser $ \s ->
    let xs = input s
    in case xs of
        [] -> (Nothing, s)
        _  -> let (xsInit, xsTail) = span p xs
              in if null xsInit
                 then (Nothing, s)
                 else (Just xsInit, s { input = xsTail })

-- | Builds a parser which parses a string until an occurrence of string `str` is
-- found.  Fails if nothing is found.
takeUntil :: String -> Parser String
takeUntil str = Parser $ \s ->
    let inp = input s
    in case findIn str inp of
        Nothing -> (Nothing, s)
        Just i  -> let (xsInit, xsTail) = splitAt i inp
                   in (Just xsInit, s { input = xsTail })

-- | Builds a parser which performs its action and then consumes any whitespace
-- after the parsed content.
spaceDelim :: Parser a -> Parser a
spaceDelim p = do
    x <- p
    takeWhile isSpace
    return x

-- | Parses a sequence of letters.
letters :: Parser String
letters = takeWhile isAlpha

-- | Parses a tokenized sequence of letters.
word :: Parser String
word = spaceDelim letters

-- | Parses a sequence of digits and returns its integer value.
number :: Parser Int
number = map read $ takeWhile isDigit

-- | Parses a specific string from the input.
litString :: String -> Parser String
litString s = do
    t <- take (length s) char
    if s == t then return t else emptyOk

-- | Returns a parser that adds an error message `msg` to the parser result
-- upon failure.
require :: Parser a -> String -> Parser a
require p msg = Parser $ \s -> case runParser p s of
    (Nothing, _) -> (Nothing, addError msg s)
    r            -> r
