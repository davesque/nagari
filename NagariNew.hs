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

-- | Parser Monoid instance.
instance Monoid (Parser a) where
    -- | The identity function for another parser when combined with `mappend`.
    mempty = Parser $ const []

    -- | Allows forking of parsing logic, concatenating the results of several
    -- parsers into one parser result.
    (Parser f) `mappend` (Parser g) = Parser $ \xs ->
        let fResult = f xs
            gResult = g xs
        in fResult ++ gResult

-- | Parser Functor instance.
instance Functor Parser where
    fmap f p = Parser $ \xs ->
        [(f y, ys) | (y, ys) <- runParser p xs]

-- | Parser Monad instance.
instance Monad Parser where
    -- | Always succeeds at parsing a value `x`.
    return x = Parser $ \xs -> [(x, xs)]

    -- | Allows for chaining of parsers together.
    Parser p >>= f = Parser $ \xs ->
        concat [runParser (f y) ys | (y, ys) <- p xs]

    -- | Always fails at parsing a value.
    fail _ = Parser $ const []

-- | Parser MonadPlus instance.
instance MonadPlus Parser where
    mzero = mempty
    mplus = mappend

----------------------
-- Parsers builders --
----------------------

-- | Succeeds at parsing a single character if the given predicate is true for
-- the parser result.
filter :: (Char -> Bool) -> Parser Char
filter p = Parser $ \xs -> case xs of
    []   -> []
    y:ys -> [(y, ys) | p y]

-- | Builds a parser which will apply itself to a string the given number of
-- times.
take :: Int -> Parser Char -> Parser String
take = replicateM

-- | Builds a parser that will succeed as long as the predicate `p` is true for
-- characters in the input stream.
takeWhile :: (Char -> Bool) -> Parser String
takeWhile p = Parser $ \xs -> case xs of
    [] -> []
    _  -> let tw = P.takeWhile p xs
              dw = P.dropWhile p xs
          in case tw of
              [] -> []
              _  -> [(tw, dw)]

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
