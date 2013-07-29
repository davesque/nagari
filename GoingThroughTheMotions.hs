import NagariNew

char' :: String -> [(Char, String)]
char' xs = case xs of
    []   -> []
    y:ys -> [(y, ys)]

return' :: String -> String -> [(String, String)]
return' xs ys = [(xs, ys)]

testOneItem :: Parser String
testOneItem = do
    a <- char
    return [a]

testOneItem' :: Parser String
testOneItem' =
    char >>= (\a ->
    return [a])

testOneItem'' :: Parser String
testOneItem'' =
    Parser char' >>= (\a ->
    Parser $ return' [a])

testOneItem''' :: Parser String
testOneItem''' =
    let g = \a -> Parser (return' [a])
    in Parser char' >>= g

testOneItem'''' :: Parser String
testOneItem'''' =
    let g = \a -> Parser $ return' [a]
    in Parser $ \xs -> concat [runParser (g y) ys | (y, ys) <- char' xs]

testOneItem''''' :: Parser String
testOneItem''''' =
    Parser $ \xs -> concat
        [ runParser ((\a -> Parser $ return' [a]) y) ys
        | (y, ys) <- char' xs ]

testTwoItem :: Parser String
testTwoItem = do
    a <- char
    b <- char
    return [a, b]

testTwoItem' :: Parser String
testTwoItem' =
    char >>= (\a ->
    char >>= (\b ->
    return [a, b]))

testTwoItem'' :: Parser String
testTwoItem'' =
    Parser char' >>= (\a ->
    Parser char' >>= (\b ->
    Parser $ return' [a, b]))

testTwoItem''' :: Parser String
testTwoItem''' =
    let g = \a -> let h = \b -> Parser $ return' [a, b]
                  in Parser char' >>= h
    in Parser char' >>= g

testTwoItem'''' :: Parser String
testTwoItem'''' =
    let g = \a -> let h = \b -> Parser $ return' [a, b]
                  in Parser $ \xs -> concat [runParser (h y) ys | (y, ys) <- char' xs]
    in Parser $ \xs -> concat [runParser (g y) ys | (y, ys) <- char' xs]

testTwoItem''''' :: Parser String
testTwoItem''''' =
    let g = \a -> Parser $ \xs -> concat [runParser ((\b -> Parser $ return' [a, b]) y) ys | (y, ys) <- char' xs]
    in Parser $ \xs -> concat [runParser (g y) ys | (y, ys) <- char' xs]

testTwoItem'''''' :: Parser String
testTwoItem'''''' =
  Parser $ \xs ->
    concat [ runParser ((\a ->
      (Parser $ \xs' ->
        concat [ runParser ((\b ->
          Parser $ return' [a, b]) y') ys'
            | (y', ys') <- char' xs' ])) y) ys
              | (y, ys) <- char' xs ]

testTwoItem''''''' :: [(String, String)]
testTwoItem''''''' =
  concat [ runParser ((\a ->
    (Parser $ \xs' ->
      concat [ runParser ((\b ->
        Parser $ return' [a, b]) y') ys'
          | (y', ys') <- char' xs'])) y) ys
            | (y, ys) <- char' "arst" ]

testTwoItem'''''''' :: [(String, String)]
testTwoItem'''''''' =
  concat [ runParser ((\a ->
    (Parser $ \xs' ->
      concat [ runParser ((\b ->
        Parser $ return' [a, b]) y') ys'
          | (y', ys') <- char' xs'])) y) ys
            | (y, ys) <- [('a', "rst")] ]

testTwoItem''''''''' :: [(String, String)]
testTwoItem''''''''' =
  runParser ((\a ->
    (Parser $ \xs' ->
      concat [ runParser ((\b ->
        Parser $ return' [a, b]) y') ys'
          | (y', ys') <- char' xs'])) 'a') "rst"

testTwoItem'''''''''' :: [(String, String)]
testTwoItem'''''''''' =
  runParser 
    (Parser $ \xs' ->
      concat [ runParser ((\b ->
        Parser $ return' ['a', b]) y') ys'
          | (y', ys') <- char' xs']) "rst"

testTwoItem''''''''''' :: [(String, String)]
testTwoItem''''''''''' =
  (\xs' -> concat [ runParser ((\b -> Parser $ return' ['a', b]) y') ys' | (y', ys') <- char' xs']) "rst"

testTwoItem'''''''''''' :: [(String, String)]
testTwoItem'''''''''''' =
  concat [ runParser ((\b -> Parser $ return' ['a', b]) y') ys' | (y', ys') <- char' "rst"]

testTwoItem''''''''''''' :: [(String, String)]
testTwoItem''''''''''''' =
  concat [ runParser ((\b -> Parser $ return' ['a', b]) y') ys' | (y', ys') <- [('r', "st")]]

testTwoItem'''''''''''''' :: [(String, String)]
testTwoItem'''''''''''''' = runParser ((\b -> Parser $ return' ['a', b]) 'r') "st"

testTwoItem''''''''''''''' :: [(String, String)]
testTwoItem''''''''''''''' = runParser (Parser $ return' ['a', 'r']) "st"

testTwoItem'''''''''''''''' :: [(String, String)]
testTwoItem'''''''''''''''' = return' ['a', 'r'] "st"

testTwoItem''''''''''''''''' :: [(String, String)]
testTwoItem''''''''''''''''' = [(['a', 'r'], "st")]
