module Nagari where

----------------
-- Data types --
----------------

-- | Parser combinator type.
newtype Parser a = Parser { runParser :: String -> [(a, String)] }

---------------
-- Instances --
---------------

-- | Parser Monad instance.
instance Monad Parser where
    -- | Always succeeds at parsing a value `x`.
    return x = Parser $ \xs -> [(x, xs)]

    -- | Allows for chaining of parsers together.
    Parser p >>= f = Parser $ \xs ->
        concat [runParser (f v) xs' | (v, xs') <- p xs]

    -- | Always fails at parsing a value.
    fail _ = Parser $ const []

-------------
-- Parsers --
-------------

-- | Parses a single character from a string.
item :: Parser Char
item = Parser $ \xs -> case xs of
    []   -> []
    y:ys -> [(y, ys)]
