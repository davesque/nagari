module Nagari where

import Control.Monad
import Data.Char
import qualified Data.List as L
import Data.Monoid
import Prelude hiding (filter, iterate, take, takeWhile, map)
import qualified Prelude as P

----------------
-- Data types --
----------------

-- | Parser combinator type.
newtype Parser a = Parser { runParser :: String -> [(a, String)] }

---------------
-- Instances --
---------------

instance Monoid (Parser a) where
    -- | The identity function for another parser when combined with `mappend`.
    mempty = Parser $ const []

    -- | Allows forking of parsing logic, concatenating the results of several
    -- parsers into one parser result.
    (Parser f) `mappend` (Parser g) = Parser $ \xs ->
        let fResult = f xs
            gResult = g xs
        in fResult ++ gResult

instance MonadPlus Parser where
    mzero = mempty
    mplus = mappend

instance Functor Parser where
    -- | Allows for mapping over parser results with a function `f`.
    fmap f p = Parser $ \xs ->
        [(f y, ys) | (y, ys) <- runParser p xs]

instance Monad Parser where
    -- | Always succeeds at parsing a value `x`.
    return x = Parser $ \xs -> [(x, xs)]

    -- | Allows for combination of parsers.
    Parser p >>= f = Parser $ \xs ->
        concat [runParser (f y) ys | (y, ys) <- p xs]

    -- | Always fails at parsing a value.
    fail _ = Parser $ const []

----------------------
-- Parsers builders --
----------------------

err :: String -> Parser a
err xs = Parser $ \ys -> error $ xs ++ " near '" ++ ys ++ "'\n"

-- | Alias for `mplus`.
and :: Parser a -> Parser a -> Parser a
and = mplus

-- | Builds a parser that first attempts to parse with a parser `p` and falls
-- back to parsing with a parser `q` on failure.
or :: Parser a -> Parser a -> Parser a
p `or` q = Parser $ \xs -> case runParser p xs of
    [] -> runParser q xs
    r  -> r

-- | Builds a parser that first attempts to parse with a parser `p` and falls
-- back to parsing with a parser `q` on failure.  Parser result type uses
-- `Either`.
or' :: Parser a -> Parser b -> Parser (Either b a)
p `or'` q = Parser $ \xs ->
    case runParser p xs of
    [] -> case runParser q xs of
          [] -> []
          r2 -> [(Left y, ys) | (y, ys) <- r2]
    r1 -> [(Right y, ys) | (y, ys) <- r1]

-- | Alias for `fmap`.
map :: (a -> b) -> Parser a -> Parser b
map = fmap

-- | Succeeds at parsing a single character if the given predicate is true for
-- the parser result.
takeOneIf :: (Char -> Bool) -> Parser Char
takeOneIf p = Parser $ \xs -> case xs of
    []   -> []
    y:ys -> [(y, ys) | p y]

takeOneIf' :: (Char -> Bool) -> Parser Char
takeOneIf' p = do
    x <- char
    if p x then return x else fail ""

-- | Builds a parser which will apply itself to a string the given number of
-- times.
take :: Int -> Parser a -> Parser [a]
take = replicateM

-- | Used as helper function by `takeAll`.
takeAll' :: Parser a -> Parser a
takeAll' p = Parser $ \xs ->
    let rs = runParser p xs
    in  rs ++ concat [runParser (takeAll' p) ys | (_, ys) <- rs]

-- | Builds a parser which will apply itself to a string until further
-- applications yield no results.
takeAll :: Parser a -> Parser [a]
takeAll p = Parser $ \xs -> case runParser (takeAll' p) xs of
    [] -> []
    rs -> let unParsed = snd . last $ rs
              results  = P.map fst rs
          in [(results, unParsed)]

-- | Builds a parser that will succeed as long as the predicate `p` is true for
-- characters in the input stream.
takeWhile :: (Char -> Bool) -> Parser String
takeWhile p = Parser $ \xs -> case xs of
    [] -> []
    _  -> let (xsInit, xsTail) = span p xs
          in [(xsInit, xsTail) | not . null $ xsInit]

-- | Finds the index of the first occurrence of a list `xs` in a list `ys`.
findIn :: (Eq a) => [a] -> [a] -> Maybe Int
findIn _ []  = Nothing
findIn [] _  = Nothing
findIn xs ys = L.elemIndex True $ L.map (L.isPrefixOf xs) (L.tails ys)

-- | Builds a parser which parses a string until an occurrence of string `s` is
-- found.  Fails if nothing is found.
takeUntil :: String -> Parser String
takeUntil s = Parser $ \xs -> case findIn s xs of
    Nothing -> []
    Just i  -> [splitAt i xs]

-- | Builds a parser which performs its action and then consumes any whitespace
-- after the parsed content.
token :: Parser a -> Parser a
token p = do
    x <- p
    takeWhile isSpace
    return x

-- | Parses a sequence of letters.
letters :: Parser String
letters = takeWhile isAlpha

-- | Parses a tokenized sequence of letters.
word :: Parser String
word = token letters

-- | Parses a sequence of digits and returns its integer value.
number :: Parser Int
number = map read $ takeWhile isDigit

-- | Parses a specific string from the input.
accept :: String -> Parser String
accept s = do
    t <- take (length s) char
    if s == t then return t else fail ""

------------------
-- Core parsers --
------------------

-- | Parses a single character.
char :: Parser Char
char = Parser $ \xs -> case xs of
    []   -> []
    y:ys -> [(y, ys)]

-- | Parses a single whitespace character.
space :: Parser Char
space = takeOneIf isSpace

-- | Parses a single alphabetical character.
alpha :: Parser Char
alpha = takeOneIf isAlpha

-- | Parses a single digit character.
digit :: Parser Char
digit = takeOneIf isDigit

-- | Parses a single alpha-numerical character.
alphaNum :: Parser Char
alphaNum = takeOneIf isAlphaNum

-- | Parses one of a given character `x`.
lit :: Char -> Parser Char
lit x = takeOneIf (==x)

-- | Succeeds at parsing a character which is not the given character `x`.
unLit :: Char -> Parser Char
unLit x = takeOneIf (/=x)
