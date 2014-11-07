module Nagari where

import Control.Monad
import Data.Char
import Prelude hiding (take)
import qualified Prelude as P


-- Position type and helper functions
------------------------------------------------------------------------------

-- | Stores information about the cursor position during parsing operations.
data Position = Position { line :: Integer, col :: Integer } deriving (Show)

-- | Increments a position to the next line.
nextLine :: Position -> Position
nextLine (Position l _c) = Position (l + 1) 0

-- | Increments a position to the next column.
nextCol :: Position -> Position
nextCol (Position l c) = Position l (c + 1)


-- Input type
------------------------------------------------------------------------------

-- | Type of data to be parsed.
type InputType = String

-- | Input wrapper type which includes meta data about cursor position.
data Input = Input {
                mark :: Position,
                -- | ^ The cursor position which marks the beginning of the
                -- current parsing operation.  This position is reset to the
                -- active cursor position whenever a value is returned in the
                -- Parser monad.
                position :: Position,
                -- | ^ The active cursor position of the parsing operation.
                -- This position is incremented whenever a character is
                -- consumed from the input.
                content :: InputType
                -- | ^ The content which is being parsed.
             } deriving (Show)


-- | Result type and helper functions
------------------------------------------------------------------------------

data Result a = Success { result :: a, unparsed :: Input }
              | Failure { reason :: String } deriving (Show)

-- | Helper function for creating `Failure` values whose messages are prefixed
-- with additional information from an `Input` value `i`.
failure :: Input -> String -> Result a
failure i msg = Failure $ "At line " ++ l ++ ", column " ++ c ++ ": " ++ msg
    where l = show . line . mark $ i
          c = show . col . mark  $ i


-- | Parser monad
------------------------------------------------------------------------------

newtype Parser a = Parser { runParser :: Input -> Result a }

instance Monad Parser where
    -- | Always succeeds at parsing the given value.
    return x = Parser $ \(Input _m p c) -> Success x (Input p p c)

    -- | Takes a parser `p` and binds its result to a function `f` which uses
    -- the result value to create and return another parser.  Allows for
    -- combining of parsers using do notation.
    p >>= f = Parser $ \i -> case runParser p i of
        Success x i' -> runParser (f x) i'
        Failure r    -> Failure r

-- | Begins a parsing operation at cursor position 0, 0 with the given parser
-- `p`.
run :: Parser a -> InputType -> Result a
run p i = runParser p (Input empty empty i)
    where empty = Position 0 0

-- | Returns a parser that always aborts with the given message.
abort :: String -> Parser a
abort msg = Parser $ \i -> failure i msg


-- Default parsers and parser builders
--------------------------------------------------------------------------------

-- | Parses a single character.
takeOne :: Parser Char
takeOne = Parser $ \i@(Input m p xs) -> case xs of
    y:ys -> if   y == '\n'
            then Success y $ Input m (nextLine p) ys
            else Success y $ Input m (nextCol p) ys
    _    -> failure i "more input expected"

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

-- | Used as helper function by `takeAll`.
takeAll' :: Parser a -> Parser a
takeAll' p = Parser $ \i ->
    let rs = runParser p i
    in  rs ++ concat [runParser (takeAll' p) ys | (_, ys) <- rs]

-- | Builds a parser which will apply itself to a string until further
-- applications yield no results.
takeAll :: Parser a -> Parser [a]
takeAll p = Parser $ \i -> case runParser p i of
    Success x i' ->
    rs -> let unParsed = snd . last $ rs
              results  = P.map fst rs
          in [(results, unParsed)]

-- | Builds a parser which will apply itself to a string until further
-- applications yield no results.
takeAll :: Parser a -> Parser [a]
takeAll p = Parser $ \xs -> case runParser (takeAll' p) xs of
    [] -> []
    rs -> let unParsed = snd . last $ rs
              results  = P.map fst rs
          in [(results, unParsed)]
