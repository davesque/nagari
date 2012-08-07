import Data.Char

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
    Just (y, ys) -> if p y then Just (y, ys) else Nothing

-- Digit
digit :: Parser Char
digit = char ? isDigit

-- Alternative parser operator
infixl 3 !
(!) :: Parser a -> Parser a -> Parser a
(m ! n) xs = case m xs of
    Nothing -> n xs
    _       -> m xs

space :: Parser Char
space = char ? isSpace

letter :: Parser Char
letter = char ? isAlpha

alphanum :: Parser Char
alphanum = letter ! digit

lit :: Char -> Parser Char
lit x = char ? (==x)

-- Parser composition operator
infixl 6 #
(#) :: Parser a -> Parser b -> Parser (a, b)
(m # n) xs = case m xs of
    Nothing      -> Nothing
    Just (y, ys) -> case n ys of
        Nothing      -> Nothing
        Just (z, zs) -> Just ((y, z), zs)

twochars :: Parser (Char, Char)
twochars = char # char

semicolon :: Parser Char
semicolon = lit ';'

becomes :: Parser (Char, Char)
becomes = twochars ? (==(':', '='))

-- Parser map operator
infixl 5 >->
(>->) :: Parser a -> (a -> b) -> Parser b
(m >-> f) xs = case m xs of
    Nothing      -> Nothing
    Just (y, ys) -> Just (f y, ys)

digitVal :: Parser Int
digitVal = digit >-> digitToInt

upcaseLetter :: Parser Char
upcaseLetter = letter >-> toUpper

sndChar :: Parser Char
sndChar = twochars >-> snd

twochars' :: Parser String
twochars' = (char # char) >-> (\(x, y) -> [x, y])

infixl 4 -#
(-#) :: Parser a -> Parser b -> Parser b
m -# n = (m # n) >-> snd

infixl 4 #-
(#-) :: Parser a -> Parser b -> Parser a
m #- n = (m # n) >-> fst

{-----
 - Applies a parser to a string `i` times and concatenates the results into an
 - array.
 -----}
iterate :: Parser a -> Int -> Parser [a]
iterate m 0 = Main.return []
iterate m i = m # Main.iterate m (i-1) >-> cons

cons :: (a, [a]) -> [a]
cons (x, xs) = x:xs

iterateWhile :: Parser a -> Parser [a]
iterateWhile m = m # iterateWhile m >-> cons ! Main.return []

letters :: Parser String
letters = letter # iterateWhile letter >-> cons

token :: Parser a -> Parser a
token m = m #- iterateWhile space

word :: Parser String
word = token letters

accept :: String -> Parser String
accept s = token (Main.iterate char (length s) ? (==s))

infix 4 #>
(#>) :: Parser a -> (a -> Parser b) -> Parser b
(m #> f) xs = case m xs of
    Nothing      -> Nothing
    Just (y, ys) -> f y ys

double :: Parser Char
double = char #> lit
