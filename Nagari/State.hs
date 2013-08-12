module Nagari.State where

-- | A data type for holding the state of a parsing operation.  Includes
-- information about the unconsumed input, the position of the cursor in the
-- input, and any errors which may have occurred.
data State =
    State { input :: String
          , col :: Int
          , line :: Int
          , errors :: [String]
          } deriving (Show)

-- | Increments the line number in a parser state by one.  Also resets the
-- column number to one.
incLine :: State -> State
incLine ps = ps { col = 0, line = line ps + 1 }

-- | Increments the column number in a parser state by one.
incCol :: State -> State
incCol ps = ps { col = col ps + 1 }

-- | Creates an initial state for a parsing operation from the given input string.
initialState :: String -> State
initialState s = State s 1 1 []

-- | Creates an error message with the given string `msg` which includes
-- information about where the error occurred according to the given parser
-- state `ps.
makeError :: String -> State -> String
makeError msg ps = "at "     ++
                   "line "   ++ show (line ps) ++ ", " ++
                   "column " ++ show (col ps)  ++ ": " ++
                   msg

-- | Adds an error message `msg` with context information to the given parser
-- `ps`.
addError :: String -> State -> State
addError msg ps = ps { errors = errors ps ++ [makeError msg ps] }
