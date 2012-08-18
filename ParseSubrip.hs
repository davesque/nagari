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

type SubNumber = Int
data SubTime = SubTime { hour :: Int
                       , minute :: Int
                       , second :: Float
                       }
type SubText = String
data SubEntry = SubEntry { index :: SubNumber
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

colon :: Parser Char
colon = lit ':'

comma :: Parser Char
comma = lit ','

twoDigitInt :: Parser Int
twoDigitInt = iterate digit 2 >>> read

-- | Doesn't really need err message since it's only used in function which
-- does have err message.
seconds :: Parser Float
seconds = token $
    iterate digit 2 #- comma # iterate digit 3
        >>> (\(whole, fractional) ->
            read $ whole ++ "." ++ fractional)

subNumber :: Parser SubNumber
subNumber = number #- newline
          ! err "Illegal subrip index number"

subTime :: Parser SubTime
subTime = twoDigitInt #- colon # twoDigitInt #- colon # seconds
    >>> (\((h, m), s) -> SubTime h m s)
    ! err "Illegal subrip time stamp"

subEntry :: Parser ((SubNumber, SubTime), SubTime)
subEntry = subNumber # subTime #- accept " --> " # subTime #- newline
