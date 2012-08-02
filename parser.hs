import Data.List.Utils -- requires MissingH

-- Expressions
data Expr =
    Num Int
    | Var String
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    deriving (Show)

-- Statements
data Statement =
    Assignment String Expr
    deriving (Show)

-- Combinator type
type Parser a = String -> Maybe (a, String)

-- Semicolon
semicolon :: Parser Char
semicolon "" = Nothing
semicolon (x:xs)
    | x == ';'  = Just (x, xs)
    | otherwise = Nothing

-- Becomes
becomes :: Parser String
becomes "" = Nothing
becomes xs
    | fst parts == ":=" = Just (fst parts, snd parts)
    | otherwise         = Nothing
    where parts = splitAt 2 xs

-- Char
char :: Parser Char
char "" = Nothing
char (x:xs) = Just (x, xs)

-- Fail
fail :: Parser a
fail xs = Nothing

-- Return
return :: a -> Parser a
return x xs = Just (x, xs)

-- Predicate decoration operator
infix 7 ?
(?) :: Parser a -> (a -> Bool) -> Parser a
(m ? p) xs = case m xs of
    Nothing      -> Nothing
    Just (y, ys) -> if p y then Just(y, ys) else Nothing
