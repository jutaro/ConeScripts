{-# LANGUAGE OverloadedStrings, LambdaCase #-}

import ConeServer.Types
import ConeServer.ConeTypes
import Parser.CSV
import IconGuesser
import ConeDemo

import qualified Data.Text              as T
import qualified Data.HashMap.Lazy      as M
import qualified Data.ByteString.Lazy   as B (writeFile)

import Data.Aeson.Encode.Pretty         (encodePretty)
import System.FilePath.Posix            (takeFileName, replaceExtension, dropExtension)
import System.Environment
import Data.List
import Data.Ord
import Data.Function
import Debug.Trace


-- order-preserving
groupByKey :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
groupByKey dict =
    let groups = groupBy ((==) `on` fst) . sortBy (comparing fst) $ dict
    in [(k, vals) | g <- groups, (not . null) g, let (k:_, vals) = unzip g]


buildFromCSV :: FilePath -> CSV -> ConeTree
buildFromCSV fName csv_@(header:_) =
    renameRoot $ attach (entry "root")
  where
    renameRoot (RoseLeaf e x ys) = RoseLeaf e {ceLabel = T.pack . dropExtension . takeFileName $ fName} x ys
    csv = case header of
      (_:"arent"):(_:"ame"):_   -> trace (concat $ "dropped header: ":header) $ tail csv_
      _                         -> csv_

    parents :: M.HashMap Text Int
    parents = M.fromList . flip zip [0..] . map (T.pack . (!! 0)) $ csv

    grouped = groupByKey rows
    rows    = flip map csv $ \case
        [par, name]     -> (parents M.! T.pack par, (T.pack name, Nothing))
        par:name:uri:_  -> (parents M.! T.pack par, (T.pack name, if "http" `isPrefixOf` uri then Just (T.pack uri) else Nothing))

    attach e @ ConeEntry {ceLabel = label} =
        RoseLeaf e {ceIsLeaf = null children} (-1) $
            map (attach . entryWithUri) children
      where
        children = fromMaybe [] (M.lookup label parents >>= flip lookup grouped)

    entryWithUri (label, uri) = (entry label) {ceTargetUri = uri}


main :: IO ()
main = do
    args <- getArgs
    let
        mIconPath = do
            ix      <- elemIndex "-i" args
            guard   $ ix < length args - 1
            return  $ args !! (ix + 1)
    if null args
        then putStrLn usage
        else do
            let fName = last args
            guesser <- newIconGuesser mIconPath
            eCsv    <- parseCSVFromFile fName
            either print (main' fName guesser) eCsv
  where
    usage = "usage: CSVImporter [-i <icon directory>] file.csv"



main' :: FilePath -> IconGuesser -> CSV -> IO ()
main' fName guesser csv =
    let
        fName'  = replaceExtension fName ".json"
        tree    = applyIconGuesser guesser $ buildFromCSV fName csv
        demo    = (fromConeTree tree) {subColorate = mkSubcolorate testColorization}
    in B.writeFile fName' (encodePretty demo)

testColorization :: [(Text, DemoColorParams)]
testColorization =
    [ ("Housing", (Just ["#e32300"], Nothing))
    , ("Bridal", (Just ["#23e300"], Nothing))
    ]
