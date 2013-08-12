module Nagari.Parser where

import Control.Monad
import Data.Char
import qualified Data.List as L
import Data.Monoid
import Prelude hiding (filter, iterate, take, takeWhile, map)
import qualified Prelude as P

import Nagari.State

type Result a = (Maybe a, State)

newtype Parser a = Parser { runParser :: State -> Result a }

instance Monad Parser where
    return = consumeOk

    Parser p >>= f = Parser $ \s ->
        let (x, s') = p s
        in case x of Just x' -> runParser (f x') s'
                     Nothing -> (Nothing, s')

consumeOk :: a -> Parser a
consumeOk x = Parser $ \s -> (Just x, s)

consumeErr :: a -> String -> Parser a
consumeErr x msg = Parser $ \s -> (Just x, addError msg s)

emptyOk :: Parser a
emptyOk = Parser $ \s -> (Nothing, s)

emptyErr :: String -> Parser a
emptyErr msg = Parser $ \s -> (Nothing, addError msg s)

---------------
-- Instances --
---------------

instance Monoid (Parser a) where
    -- | The identity function for another parser when combined with `mappend`.
    mempty = emptyOk

    -- | Allows running of an alternative parser if the first parser fails
    -- parsers into one parser result.
    p `mappend` q = Parser $ \s ->
        case runParser p s of
            (Nothing, _) -> runParser q s
            r            -> r

instance MonadPlus Parser where
    mzero = mempty
    mplus = mappend

instance Functor Parser where
    -- | Allows for mapping over parser results with a function `f`.
    fmap f p = Parser $ \s ->
        let (x, s') = runParser p s in (fmap f x, s')

------------------
-- Core parsers --
------------------

-- | Parses one character from the input stream.
char :: Parser Char
char = Parser $ \s -> case input s of
    []   -> (Nothing, s)
    x:xs -> let posModifier = if x == '\n' then incLine else incCol
            in (Just x, posModifier s { input = xs })

-- | Looks ahead one character in the input stream.
lookAhead :: Parser Char
lookAhead = Parser $ \s -> case input s of
    []  -> (Nothing, s)
    y:_ -> (Just y, s)

-- | Succeeds at parsing a single character if the given predicate is true for
-- the parser result.
charIf :: (Char -> Bool) -> Parser Char
charIf p = do
    x <- lookAhead
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
lit :: Char -> Parser Char
lit x = charIf (==x)

-- | Succeeds at parsing a character which is not the given character `x`.
unLit :: Char -> Parser Char
unLit x = charIf (/=x)

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
-- characters in the input stream.
takeWhile :: (Char -> Bool) -> Parser String
takeWhile p = Parser $ \s ->
    let xs = input s
    in case xs of
        [] -> (Nothing, s)
        _  -> let (xsInit, xsTail) = span p xs
              in if null xsInit
                 then (Nothing, s)
                 else (Just xsInit, s { input = xsTail })

-- | Finds the index of the first occurrence of a list `xs` in a list `ys`.
findIn :: (Eq a) => [a] -> [a] -> Maybe Int
findIn _ []  = Nothing
findIn [] _  = Nothing
findIn xs ys = L.elemIndex True $ L.map (L.isPrefixOf xs) (L.tails ys)

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
    if s == t then return t else emptyOk
