import Data.Char

{-
 - Returns a tuple of its two arguments.
 -}
build :: a -> b -> (a, b)
build x y = (x, y)

{-
 - Applies the cons operator to the members of a double.
 -}
cons :: (a, [a]) -> [a]
cons (x, xs) = x:xs

{-
 - Language expression type.
 -}
data Expr =
    Num Int
    | Var String
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    deriving (Show)

{-
 - Language statement type.
 -}
data Statement =
    Assignment String Expr
    deriving (Show)

{-
 - Parser (combinator) type.
 -}
type Parser a = String -> Maybe (a, String)

{-
 - Filters the result of a parser `p` with a boolean function `f`.
 -}
infix 7 `pfilter`
pfilter :: Parser a -> (a -> Bool) -> Parser a
pfilter p f xs = case p xs of
    Nothing -> Nothing
    r@(Just (y, ys)) -> if f y then r else Nothing
infix 7 ?
(?) = pfilter

{-
 - Returns the result of an alternative parser `q` if a parser `p` fails.
 -}
infixl 3 `palternative`
palternative :: Parser a -> Parser a -> Parser a
palternative p q xs = case p xs of
    Nothing -> q xs
    _       -> p xs
infixl 3 !
(!) = palternative

{-
 - Maps a function `f` over the parsed portion of the result of a parser `p`.
 -}
infixl 5 `pmap`
pmap :: Parser a -> (a -> b) -> Parser b
pmap p f xs = case p xs of
    Nothing      -> Nothing
    Just (y, ys) -> Just (f y, ys)
infixl 5 >->
(>->) = pmap

{-
 - Provides the result of a parser `p` to another parser which is returned by a function `f`.
 -}
infix 4 `pbind`
pbind :: Parser a -> (a -> Parser b) -> Parser b
pbind p f xs = case p xs of
    Nothing      -> Nothing
    Just (y, ys) -> f y ys
infix 4 #>
(#>) = pbind

{-
 - Concatenates the results of two parsers `p` and `q` into a tuple.
 -}
infixl 6 `pcat`
pcat :: Parser a -> Parser b -> Parser (a, b)
pcat m n = m #> (\x -> n >-> build x)
infixl 6 #
(#) = pcat

{-
 - Returns the result of the first of two parsers `p` and `q`.
 -}
infixl 4 `psnd`
psnd :: Parser a -> Parser b -> Parser b
psnd m n = (m # n) >-> snd
infixl 4 -#
(-#) = psnd

{-
 - Returns the result of the second of two parsers `p` and `q`.
 -}
infixl 4 `pfst`
pfst :: Parser a -> Parser b -> Parser a
pfst m n = (m # n) >-> fst
infixl 4 #-
(#-) = pfst

{-
 - Always succeeds in parsing a value `x`.
 -}
return :: a -> Parser a
return x xs = Just (x, xs)

{-
 - Always fails to parse.
 -}
fail :: Parser a
fail xs = Nothing

{-
 - Applies a parser to a string `i` times and concatenates the results into an
 - array.
 -}
iterate :: Parser a -> Int -> Parser [a]
iterate m 0 = Main.return []
iterate m i = m # Main.iterate m (i-1) >-> cons

{-
 - Parses a string while a parser `m` succeeds and returns all results as an
 - array.
 -}
iterateWhile :: Parser a -> Parser [a]
iterateWhile m = m # iterateWhile m >-> cons ! Main.return []

{-
 - Parses a single char.
 -}
char :: Parser Char
char "" = Nothing
char (x:xs) = Just (x, xs)

{-
 - Parses a single char that is a digit.
 -}
digit :: Parser Char
digit = char ? isDigit

{-
 - Parses a single char that is whitespace.
 -}
space :: Parser Char
space = char ? isSpace

{-
 - Parses a single char that is alphabetical.
 -}
letter :: Parser Char
letter = char ? isAlpha

{-
 - Parses a single char that is alphanumeric.
 -}
alphanum :: Parser Char
alphanum = letter ! digit

{-
 - Parses a single char `x`.
 -}
lit :: Char -> Parser Char
lit x = char ? (==x)

{-
 - Parses two chars.
 -}
twochars :: Parser (Char, Char)
twochars = char # char

{-
 - Parses a semicolon.
 -}
semicolon :: Parser Char
semicolon = lit ';'

{-
 - Parses an assignment operator (':=').
 -}
becomes :: Parser (Char, Char)
becomes = twochars ? (==(':', '='))

{-
 - Parses a single digit char and converts it to an integer value.
 -}
digitVal :: Parser Int
digitVal = digit >-> digitToInt

{-
 - Parses a single alphabetical char and converts it to uppercase.
 -}
upcaseLetter :: Parser Char
upcaseLetter = letter >-> toUpper

{-
 - Returns the second char of two parsed chars.
 -}
sndChar :: Parser Char
sndChar = twochars >-> snd

{-
 - Returns two parsed chars as a string.
 -}
twochars' :: Parser String
twochars' = (char # char) >-> (\(x, y) -> [x, y])

{-
 - Parses a sequence of letters.
 -}
letters :: Parser String
letters = letter # iterateWhile letter >-> cons

{-
 - Converts a parser `m` into a parser which will eat any whitespace after that
 - which if successfully parsed.
 -}
token :: Parser a -> Parser a
token m = m #- iterateWhile space

word :: Parser String
word = token letters

accept :: String -> Parser String
accept s = token (Main.iterate char (length s) ? (==s))

{- 
 - Takes one char and passes the result to lit, which parses one more char.
 - Returns only one instance of that char.
 -}
double :: Parser Char
double = char #> lit

number :: Parser Int
number = token (iterateWhile digit) >-> read

var :: Parser Expr
var = word >-> Var

num :: Parser Expr
num = number >-> Num
