import Test.QuickCheck
import qualified Nagari as N

-- | Char should always produce a result list of length one for non-empty input
-- strings.
prop_charResultSize1 :: String -> Property
prop_charResultSize1 xs = not (null xs) ==> length result == 1
    where result = N.runParser N.char xs

-- | Char should always produce a result list of length zero for empty input
-- strings.
prop_charResultSize2 :: String -> Property
prop_charResultSize2 xs = null xs ==> null result
    where result = N.runParser N.char xs

-- | The unparsed portion of a result from char should always be the tail of
-- the input string.
prop_charParsed :: String -> Property
prop_charParsed xs = not (null xs) ==> head xs == (fst . head $ result)
    where result = N.runParser N.char xs

-- | The parsed portion of a result from char should always be the head of the
-- input string.
prop_charUnparsed :: String -> Property
prop_charUnparsed xs = not (null xs) ==> tail xs == (snd . head $ result)
    where result = N.runParser N.char xs

main :: IO ()
main = do
    quickCheck prop_charResultSize1
    quickCheck prop_charResultSize2
    quickCheck prop_charUnparsed
    quickCheck prop_charParsed
