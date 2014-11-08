module Nagari where

import Control.Monad
import Data.Char
import Prelude hiding (take)
import qualified Prelude as P


-- Position type and helper functions
------------------------------------------------------------------------------

-- | Type of data to be parsed.
type InputType = String

-- | Stores information about the cursor position during parsing operations.
data Position = Position { line :: Integer, col :: Integer, remaining :: InputType } deriving (Show)

-- | Move the cursor in a position appropriately according to the given
-- character.
moveCursor :: Char -> Position -> Position
moveCursor x (Position l c r)
    | x == '\n' = Position (l + 1) 0 r
    | otherwise = Position l (c + 1) r


-- Input type
------------------------------------------------------------------------------

-- | Input wrapper type which includes meta data about cursor position.
data Input = Input {
                rewind :: Position,
                -- | ^ The cursor position which marks the beginning of the
                -- current parsing operation.  This position is reset to the
                -- active cursor position whenever a value is returned in the
                -- Parser monad.
                current :: Position
                -- | ^ The active cursor position of the parsing operation.
                -- This position is incremented whenever a character is
                -- consumed from the input.
             } deriving (Show)


-- | Result type and helper functions
------------------------------------------------------------------------------

data Result a = Success { result :: a, unparsed :: Input }
              | Failure { reason :: String, unparsed :: Input } deriving (Show)

-- | Helper function for creating `Failure` values whose messages are prefixed
-- with additional information from an `Input` value `i`.
failure :: String -> Input -> Result a
failure msg i = Failure ("At line " ++ l ++ ", column " ++ c ++ ": " ++ msg) i
    where l = show . line . rewind $ i
          c = show . col . rewind  $ i


-- | Parser monad
------------------------------------------------------------------------------

newtype Parser a = Parser { runParser :: Input -> Result a }

instance Monad Parser where
    -- | Always succeeds at parsing the given value.
    return x = Parser $ \(Input _r c) -> Success x (Input c c)

    -- | Takes a parser `p` and binds its result to a function `f` which uses
    -- the result value to create and return another parser.  Allows for
    -- combining of parsers using do notation.
    p >>= f = Parser $ \i -> case runParser p i of
        Success x i'             -> runParser (f x) i'
        Failure rsn (Input r _c) -> Failure rsn (Input r r)

-- | Begins a parsing operation at cursor position 0, 0 with the given parser
-- `p`.
run :: Parser a -> InputType -> Result a
run p i = runParser p (Input start start)
    where start = Position 0 0 i

-- | Returns a parser that always aborts with the given message.
abort :: String -> Parser a
abort msg = Parser $ \i -> failure msg i


-- Default parsers and parser builders
--------------------------------------------------------------------------------

-- | Parses a single character.
takeOne :: Parser Char
takeOne = Parser $ \i@(Input r (Position l c xs)) -> case xs of
    y:ys -> let crnt = moveCursor y $ Position l c ys
            in  Success y $ Input r crnt
    _    -> failure "more input expected" i

-- | Parses a single character if the given predicate `p` returns `True` when
-- applied to the parsed result.
takeOneIf :: (Char -> Bool) -> String -> Parser Char
takeOneIf p msg = do
    x <- takeOne
    if p x then return x else abort msg

-- | Parses a single alphabetical character.
alpha :: Parser Char
alpha = takeOneIf isAlpha "expected alphabetical character"

take :: Int -> Parser a -> Parser [a]
take = replicateM

-- | Builds a parser which will apply itself to a string until further
-- applications yield no results.
takeAll :: Parser a -> Parser [a]
takeAll p = Parser $ \i -> case runParser p i of
    Success x i'  -> let next = runParser (takeAll p) i'
                     in Success ([x] ++ (result next)) (unparsed next)
    Failure _r i' -> Success [] i'
