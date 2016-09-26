-----------------------------------------------------------------------------
--
-- Module      :  Parser.CSV
-- Copyright   :  All rights reserver
-- License     :  AllRightsReserved
--
-- Maintainer  :  ICS AG
-- Stability   :
-- Portability :
--
-- | Reused from hackage package csv to get semicolon separated format
--
-----------------------------------------------------------------------------

module Parser.CSV (CSV
                 , Record
                 , Field
                 , csv
                 , parseCSV
                 , parseCSVFromFile
                 , parseCSVTest
                 , printCSV
                 , printCSVUnescaped
                 ) where

import Text.ParserCombinators.Parsec
import Data.List (intersperse, intercalate)
import Data.Char (isSpace)
import System.IO (hGetContents, utf8, hSetEncoding, openFile)
import GHC.IO.IOMode (IOMode(..))

-- | A CSV file is a series of records. According to the RFC, the
-- records all have to have the same length. As an extension, I
-- allow variable length records.
type CSV = [Record]

-- | A record is a series of fields
type Record = [Field]

-- | A field is a string
type Field = String

sepChar = ','

-- | A Parsec parser for parsing CSV files
csv :: Parser CSV
csv = do x <- record `sepEndBy` many1 (oneOf "\n\r")
         eof
         return x

record :: Parser Record
record = do
    r <- (quotedField <|> field) `sepBy` char sepChar
    return (map trim r)

field :: Parser Field
field = many (noneOf (sepChar : "\n\r\""))

quotedField :: Parser Field
quotedField = between (char '"') (char '"') $
              many (noneOf "\"" <|> try (string "\"\"" >> return '"'))

-- | Given a file name (used only for error messages) and a string to
-- parse, run the parser.
parseCSV :: FilePath -> String -> Either ParseError CSV
parseCSV = parse csv

-- | Given a file name, read from that file and run the parser
parseCSVFromFile :: FilePath -> IO (Either ParseError CSV)
parseCSVFromFile fp = do
    hdl <- openFile fp ReadMode
    hSetEncoding hdl utf8
    str <- hGetContents hdl
    return (parse csv fp str)

-- | Given a string, run the parser, and print the result on stdout.
parseCSVTest :: String -> IO ()
parseCSVTest = parseTest csv

-- | Given an object of type CSV, generate a CSV formatted
-- string. Always uses escaped fields.
printCSV :: CSV -> String
printCSV records = unlines (printRecord `map` records)
    where printRecord = intercalate [sepChar] . map printField
          printField f = "\"" ++ concatMap escape f ++ "\""
          escape '"' = "\"\""
          escape x = [x]

-- | Given an object of type CSV, generate a CSV formatted
-- string. Always uses escaped fields.
printCSVUnescaped :: CSV -> String
printCSVUnescaped records = unlines (printRecord `map` records)
    where printRecord = intercalate [sepChar] . map printField
          printField f = f


-- | remove leading and trailing spaces
trim :: String -> String
trim      = f . f
   where f = reverse . dropWhile isSpace
