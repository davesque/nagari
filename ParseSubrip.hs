module ParseSubrip where

import Prelude hiding (return, iterate)
import Data.Char
import Data.Map (Map, fromList, insert)
import Data.List.Utils
import Text.Printf

import ParseCore

-------------------
-- Grammar types --
-------------------

type SubIndex = Int
data SubTime = SubTime { hour :: Int
                       , minute :: Int
                       , second :: Float
                       }
type SubText = String
data SubEntry = SubEntry { index :: SubIndex
                         , startTime :: SubTime
                         , endTime :: SubTime
                         , text :: SubText
                         }

instance Show SubTime where
    show (SubTime hour minute second) =
        fHour   ++ ":" ++
        fMinute ++ ":" ++
        replace "." "," fSecond
        where fHour   = printf "%02d" hour
              fMinute = printf "%02d" minute
              fSecond = printf "%06.3f" second

instance Show SubEntry where
    show (SubEntry number startTime endTime text) =
        fNumber ++ "\n" ++
        fTime   ++ "\n" ++
        text    ++ "\n"
        where fNumber = show number
              fTime   = show startTime ++ " --> " ++ show endTime

---------------------
-- Grammar parsers --
---------------------

newline :: Parser Char
newline = lit '\n'

notNewline :: Parser Char
notNewline = unlit '\n'

colon :: Parser Char
colon = lit ':'

comma :: Parser Char
comma = lit ','

twoDigitInt :: Parser Int
twoDigitInt = iterate digit 2 >>> read

nonEmptyLine :: Parser String
nonEmptyLine = iterateWhile notNewline #- newline

-- | Doesn't really need err message since only used in function which has err
-- message.
seconds :: Parser Float
seconds = iterate digit 2 #- comma # iterate digit 3
    >>> (\(whole, fractional) -> read $ whole ++ "." ++ fractional)

subNumber :: Parser SubIndex
subNumber = number #- newline

subTime :: Parser SubTime
subTime = twoDigitInt #- colon # twoDigitInt #- colon # seconds
    >>> (\((h, m), s) -> SubTime h m s)

subRange :: Parser (SubTime, SubTime)
subRange = subTime #- accept " --> " # subTime #- newline

subText :: Parser SubText
subText = (iterateUntil "\n\n" >>> (++"\n")) #- double

subEntry :: Parser SubEntry
subEntry = subNumber # subRange # subText
    >>> (\((ind, (t1, t2)), text) -> SubEntry ind t1 t2 text)
