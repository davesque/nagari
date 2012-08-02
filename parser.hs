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
