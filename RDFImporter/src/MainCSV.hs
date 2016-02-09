
import ConeServer.Types
import ConeServer.ConeTypes
import Parser.CSV
import IconGuesser

import Data.Aeson.Encode.Pretty (encodePretty)
import System.Environment
import Data.List (findIndex)


main :: IO ()
main = do
    args <- getArgs
    let
        mIconPath = do
            ix      <- findIndex (== "-i") args
            guard   $ ix < length args - 1
            return  $ args !! (ix + 1)
    if null args
        then putStrLn usage
        else do
            let fName = last args
            guesser <- newIconGuesser mIconPath
            eCsv    <- parseCSVFromFile fName
            either (putStrLn . show) (main' fName guesser) eCsv
  where
    usage = "usage: CSVImporter [-i <icon directory>] file.csv"

main' :: FilePath -> IconGuesser -> CSV -> IO ()
main' fName csv guesser =
    return ()
