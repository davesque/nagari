{-
 - A SubRip file is:
 - * A collection of sub rip entries
 -
 - A collection of sub rip entries is:
 - * Sub rip entries in a file separated by a newline character
 -
 - A sub rip entry is:
 - * A number then a newline, then
 - * Two time stamps separated by ' --> ' then a newline, then
 - * Any number of lines of text ending with newline characters
 -}

import ParseCore
import ParseSubrip
import Control.Exception
import Data.Char
import Data.Fixed
import Data.List.Utils
import System.Directory
import System.Environment
import System.IO
import Text.Printf

{-
 - Data Building/Muting Functions
 -}
getTotalSeconds :: SubTime -> Float
getTotalSeconds (SubTime h m s) = fromIntegral h * 3600 + fromIntegral m * 60 + s

shiftSubTime :: Float -> SubTime -> SubTime
shiftSubTime interval entry =
    SubTime newHours newMinutes newSeconds
    where newTotalSeconds = max 0 (getTotalSeconds entry + interval)
          newHours        = floor $ newTotalSeconds / 3600
          newMinutes      = floor $ (newTotalSeconds `mod'` 3600) / 60
          newSeconds      = newTotalSeconds `mod'` 60

shiftSubEntry :: Float -> SubEntry -> SubEntry
shiftSubEntry interval (SubEntry number startTime endTime text) =
    SubEntry number newStartTime newEndTime text
    where newStartTime = shiftSubTime interval startTime
          newEndTime   = shiftSubTime interval endTime

cleanEntries :: Maybe ([SubEntry], String) -> [SubEntry]
cleanEntries Nothing = error "Illegal subrip entry found!"
cleanEntries (Just (es, _)) = es

{-
 - IO/Main
 -}
shiftEntries :: Float -> String -> IO ()
shiftEntries interval filename = do
    contents <- readFile filename

    let maybeEntries = iterateWhile subEntry contents
        entries      = cleanEntries maybeEntries
        newEntries   = map (shiftSubEntry interval) entries

    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            mapM_ (hPutStr tempHandle . show) newEntries
            hClose tempHandle
            removeFile filename
            renameFile tempName filename)

    putStrLn "Done!"

printUsage :: IO ()
printUsage = do
    putStrLn "Usage:"
    putStrLn "timeshifter interval filename"

dispatch :: [String] -> IO ()
dispatch (interval:filename:[]) = shiftEntries (read interval) filename
dispatch _ = printUsage

main = do
    args <- getArgs
    dispatch args
