module NagariNew where

import Control.Monad
import Data.Char
import Data.Monoid
import Prelude hiding (filter, iterate, take, takeWhile)
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

-- | Succeeds at parsing a single character if the given predicate is true for
-- the parser result.
filter :: (Char -> Bool) -> Parser Char
filter p = Parser $ \xs -> case xs of
    []   -> []
    y:ys -> [(y, ys) | p y]

filter' :: (Char -> Bool) -> Parser Char
filter' p = do
    x <- char
    if p x then return x else fail ""

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

-- | Builds a parser which will apply itself to a string the given number of
-- times.
take :: Int -> Parser Char -> Parser String
take = replicateM

{-iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a-}
{-iterateUntilM p f v -}
    {-| p v       = return v-}
    {-| otherwise = f v >>= iterateUntilM p f-}

{--- |Execute an action repeatedly until its result satisfies a predicate,-}
{--- and return that result (discarding all others).-}
{-iterateUntil :: Monad m => (a -> Bool) -> m a -> m a-}
{-iterateUntil p x = x >>= iterateUntilM p (const x)-}

-- | Builds a parser that will succeed as long as the predicate `p` is true for
-- characters in the input stream.
takeWhile :: (Char -> Bool) -> Parser String
takeWhile p = Parser $ \xs -> case xs of
    [] -> []
    _  -> let (xsInit, xsTail) = span p xs
          in [(xsInit, xsTail) | not . null $ xsInit]

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
space = filter isSpace

-- | Parses a single alphabetical character.
alpha :: Parser Char
alpha = filter isAlpha

-- | Parses a single digit character.
digit :: Parser Char
digit = filter isDigit

-- | Parses a single alpha-numerical character.
alphaNum :: Parser Char
alphaNum = filter isAlphaNum

-- | Parses one of a given character `x`.
lit :: Char -> Parser Char
lit x = filter (==x)

-- | Succeeds at parsing a character which is not the given character `x`.
unLit :: Char -> Parser Char
unLit x = filter (/=x)
