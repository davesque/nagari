import Test.QuickCheck
import qualified Nagari as N

-- | The unparsed portion of a result from char should never be longer than the
-- input string.
prop_char_growth :: String -> Bool
prop_char_growth xs = all (<= length xs) resultLengths
    where result = N.runParser N.char xs
          resultLengths = map (length . snd) result

prop_resultOfCharForNonEmptyHasLengthOne :: String -> Property
prop_resultOfCharForNonEmptyHasLengthOne xs = not (null xs) ==> length result == 1
    where result = N.runParser N.char xs

prop_resultOfCharForEmptyIsNull :: String -> Property
prop_resultOfCharForEmptyIsNull xs = null xs ==> null result
    where result = N.runParser N.char xs

prop_unparseResultOfCharForNonEmptyIsOneShorter :: String -> Bool
prop_unparseResultOfCharForNonEmptyIsOneShorter [] = True
prop_unparseResultOfCharForNonEmptyIsOneShorter xs = origLength - 1 == unparsedLength
    where result = N.runParser N.char xs
          origLength = length xs
          unparsedLength = length . snd . head $ result

main :: IO ()
main = do
    quickCheck prop_unparsedResultOfCharIsNeverLonger
    quickCheck prop_resultOfCharForNonEmptyHasLengthOne
    quickCheck prop_resultOfCharForEmptyIsNull
    quickCheck prop_unparseResultOfCharForNonEmptyIsOneShorter
