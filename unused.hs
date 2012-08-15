-- | Parses two chars.
twochars :: Parser (Char, Char)
twochars = char # char

-- | Returns the second char of two parsed chars.
sndChar :: Parser Char
sndChar = twochars >>> snd

-- | Returns two parsed chars as a string.
twochars' :: Parser String
twochars' = char # char >>> (\(x, y) -> [x, y])

-- | Parses a semicolon.
semicolon :: Parser Char
semicolon = lit ';'

-- | Parses a single digit char and converts it to an integer value.
digitVal :: Parser Int
digitVal = digit >>> digitToInt

-- | Parses a single alphabetical char and converts it to uppercase.
upcaseLetter :: Parser Char
upcaseLetter = letter >>> toUpper

-- | Parses a string s as a token.
accept :: String -> Parser String
accept s = token $ iterate char (length s) ? (==s)

-- | Parses two chars of the same value.  The `char` parser parses one char,
-- which is then passed to `lit` to create another parser which will accept the
-- same char.
double :: Parser Char
double = char >>- lit
