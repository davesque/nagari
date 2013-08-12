module Nagari.Utils
    ( findIn ) where

import Data.List

-- | Finds the index of the first occurrence of a list `xs` in a list `ys`.
findIn :: (Eq a) => [a] -> [a] -> Maybe Int
findIn _ []  = Nothing
findIn [] _  = Nothing
findIn xs ys = elemIndex True $ map (isPrefixOf xs) (tails ys)
