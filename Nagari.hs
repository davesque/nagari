module Nagari where

import Control.Monad
import Data.Char
import qualified Data.List as L
import Prelude hiding (takeWhile, take)

-- Result type
------------------------------------------------------------------------------

data Result a = Failure { parsed :: ShowS, unParsed :: String, errorDesc :: String }
              | Success { parsed :: ShowS, unParsed :: String, result :: a }

instance (Show a) => Show (Result a) where
    show (Failure p up ed) =
        concat [ "Failure {parsed = \""
               , p ""
               , "\", unParsed = \""
               , up
               , "\", desc = \""
               , ed
               , "\"}" ]
    show (Success p up r) =
        concat [ "Success {parsed = \""
               , p ""
               , "\", unParsed = \""
               , up
               , "\", result = "
               , show r
               , "}" ]

-- | Constructs a parsing failure value with a located error message.
failure :: ShowS -> String -> String -> Result a
failure p up msg = Failure p up $ makeErrorMessage p msg

-- | Constructs an error message for a parsing failure which includes
-- information about the error's location.
makeErrorMessage :: ShowS -> String -> String
makeErrorMessage p msg = "At " ++ locationDesc ++ ": " ++ msg
    where (line, col)   = getErrorLocation (p "")
          locationDesc  = "line " ++ show line ++ ", column " ++ show col

-- | Gets a tuple with the current line and column number for a parsing
-- operation based on the parsed input.
getErrorLocation :: String -> (Int, Int)
getErrorLocation p    = (lineNo, colNo)
    where parsedLines = splitLines p
          lineNo      = length parsedLines
          colNo       = (length $ last parsedLines) + 1

-- | Splits a string by newline characters.
splitLines :: String -> [String]
splitLines = (`splitOnElem` '\n')

-- | Splits a list `xs` on occurrences of an element `delim`.
splitOnElem :: (Eq a) => [a] -> a -> [[a]]
xs `splitOnElem` delim = case splits of
    Just (left, right) -> concat [[left], (tail right) `splitOnElem` delim]
    Nothing            -> [xs]
    where delimIndex = L.elemIndex delim xs
          splits     = delimIndex >>= \i -> return $ splitAt i xs

-- | Splits a list `xs` on occurrences of another list `delim`.
splitOnList :: (Eq a) => [a] -> [a] -> [[a]]
xs `splitOnList` delim = case splits of
    Just (left, right) -> let rightWithoutDelim = drop (length delim) right
                          in concat [[left], rightWithoutDelim `splitOnList` delim]
    Nothing            -> [xs]
    where delimIndices    = map (delim `L.isPrefixOf`) (L.tails xs)
          firstDelimIndex = L.elemIndex True delimIndices
          splits          = firstDelimIndex >>= \i -> return $ splitAt i xs


-- | Input and Parser types
------------------------------------------------------------------------------

type InputParsed   = ShowS
type InputUnParsed = String
type Input = (InputParsed, InputUnParsed)

newtype Parser a = Parser { runParser :: Input -> Result a }

instance Functor Parser where
    -- | Allows for mapping over parser results with a function `f`.
    fmap f parser = Parser $ \(p, up) -> case runParser parser (p, up) of
        Success p' up' r  -> Success p' up' (f r)
        Failure p' up' ed -> Failure p' up' ed

instance Monad Parser where
    return x = Parser $ \(p, up) -> Success p up x

    parser >>= f = Parser $ \(p, up) -> case runParser parser (p, up) of
        Failure p' up' ed -> Failure p' up' ed
        Success p' up' r  -> runParser (f r) (p', up')


-- Parser builders
--------------------------------------------------------------------------------

-- | Builds a parser that first attempts to parse with a parser `p` and falls
-- back to parsing with a parser `q` on failure.
or :: Parser a -> Parser a -> Parser a
p `or` q = Parser $ \(pa, unPa) -> case runParser p (pa, unPa) of
    Failure _ _ _ -> runParser q (pa, unPa)
    r             -> r


-- Primitive parsers
------------------------------------------------------------------------------

anyChar :: Parser Char
anyChar = Parser $ \(p, up) -> case up of
    []   -> failure p up "Expected at least one character"
    x:xs -> Success (p . showChar x) xs x

ensure :: Int -> Parser String
ensure n = Parser $ \(p, up) ->
    if   length up < n
    then failure p up ("Expected at least " ++ show n ++ " characters")
    else runParser (replicateM n anyChar) (p, up)

err :: String -> Parser a
err msg = Parser $ \(p, up) -> failure p up msg

-- | Succeeds at parsing a single character if the given predicate is true for
-- the parser result.
takeOneIf :: (Char -> Bool) -> Parser Char
takeOneIf p = do
    x <- anyChar
    if p x then return x else err "Unexpected character"

-- | Builds a parser which will apply itself to a string the given number of
-- times.
{-take :: Int -> Parser a -> Parser [a]-}
{-take = replicateM-}

{--- | Used as helper function by `takeAll`.-}
{-takeAll' :: Parser a -> Parser a-}
{-takeAll' p = Parser $ \xs ->-}
    {-let rs = runParser p xs-}
    {-in  rs ++ concat [runParser (takeAll' p) ys | (_, ys) <- rs]-}

{--- | Builds a parser which will apply itself to a string until further-}
{--- applications yield no results.-}
{-takeAll :: Parser a -> Parser [a]-}
{-takeAll p = Parser $ \xs -> case runParser (takeAll' p) xs of-}
    {-[] -> []-}
    {-rs -> let unParsed = snd . last $ rs-}
              {-results  = map fst rs-}
          {-in [(results, unParsed)]-}

{--- | Builds a parser that will succeed as long as the predicate `p` is true for-}
{--- characters in the input stream.-}
{-takeWhile :: (Char -> Bool) -> Parser String-}
{-takeWhile p = Parser $ \xs -> case xs of-}
    {-[] -> []-}
    {-_  -> let (xsInit, xsTail) = span p xs-}
          {-in [(xsInit, xsTail) | not . null $ xsInit]-}

{--- | Finds the index of the first occurrence of a list `xs` in a list `ys`.-}
{-findIn :: (Eq a) => [a] -> [a] -> Maybe Int-}
{-findIn _ []  = Nothing-}
{-findIn [] _  = Nothing-}
{-findIn xs ys = L.elemIndex True $ map (xs `L.isPrefixOf`) (L.tails ys)-}

{--- | Builds a parser which parses a string until an occurrence of string `s` is-}
{--- found.  Fails if nothing is found.-}
{-takeUntil :: String -> Parser String-}
{-takeUntil s = Parser $ \xs -> case findIn s xs of-}
    {-Nothing -> []-}
    {-Just i  -> [splitAt i xs]-}

{--- | Builds a parser which performs its action and then consumes any whitespace-}
{--- after the parsed content.-}
{-token :: Parser a -> Parser a-}
{-token p = do-}
    {-x <- p-}
    {-takeWhile isSpace-}
    {-return x-}

{--- | Parses a sequence of letters.-}
{-letters :: Parser String-}
{-letters = takeWhile isAlpha-}

{--- | Parses a tokenized sequence of letters.-}
{-word :: Parser String-}
{-word = token letters-}

{--- | Parses a sequence of digits and returns its integer value.-}
{-number :: Parser Int-}
{-number = map read $ takeWhile isDigit-}

{--- | Parses a specific string from the input.-}
{-accept :: String -> Parser String-}
{-accept s = do-}
    {-t <- take (length s) char-}
    {-if s == t then return t else fail ""-}

{--------------------}
{--- Core parsers ---}
{--------------------}

{--- | Parses a single character.-}
{-char :: Parser Char-}
{-char = Parser $ \xs -> case xs of-}
    {-[]   -> []-}
    {-y:ys -> [(y, ys)]-}

{--- | Parses a single whitespace character.-}
{-space :: Parser Char-}
{-space = takeOneIf isSpace-}

{--- | Parses a single alphabetical character.-}
{-alpha :: Parser Char-}
{-alpha = takeOneIf isAlpha-}

{--- | Parses a single digit character.-}
{-digit :: Parser Char-}
{-digit = takeOneIf isDigit-}

{--- | Parses a single alpha-numerical character.-}
{-alphaNum :: Parser Char-}
{-alphaNum = takeOneIf isAlphaNum-}

{--- | Parses one of a given character `x`.-}
{-lit :: Char -> Parser Char-}
{-lit x = takeOneIf (==x)-}

{--- | Succeeds at parsing a character which is not the given character `x`.-}
{-unLit :: Char -> Parser Char-}
{-unLit x = takeOneIf (/=x)-}
