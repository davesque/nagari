module Nagari.Parser where

import Control.Monad
import Data.Monoid

import Nagari.State

-- | Parser result type.
type Result a = (Maybe a, State)

-- | Parser combinator type.
newtype Parser a = Parser { runParser :: State -> Result a }

-- | Returns a parser that will consume the given token with no errors.
consumeOk :: a -> Parser a
consumeOk x = Parser $ \s -> (Just x, s)

-- | Returns a parser that will consume the given token with the given error
-- message.
consumeErr :: a -> String -> Parser a
consumeErr x msg = Parser $ \s -> (Just x, addError msg s)

-- | Returns a parser that will consume nothing with no errors.
emptyOk :: Parser a
emptyOk = Parser $ \s -> (Nothing, s)

-- | Returns a parser that will consume nothing with the given error message.
emptyErr :: String -> Parser a
emptyErr msg = Parser $ \s -> (Nothing, addError msg s)

---------------
-- Instances --
---------------

instance Monad Parser where
    return = consumeOk

    Parser p >>= f = Parser $ \s ->
        let (x, s') = p s
        in case x of Just x' -> runParser (f x') s'
                     Nothing -> (Nothing, s')

instance Monoid (Parser a) where
    mempty = emptyOk

    -- | Allows running of an alternative parser `q` if the given parser `p`
    -- fails.
    p `mappend` q = Parser $ \s ->
        case runParser p s of
            (Nothing, _) -> runParser q s
            r            -> r

instance MonadPlus Parser where
    mzero = mempty
    mplus = mappend

instance Functor Parser where
    -- | Allows modification of the result of a parser `p` by application of a
    -- function `f`.
    fmap f p = Parser $ \s ->
        let (x, s') = runParser p s in (fmap f x, s')
