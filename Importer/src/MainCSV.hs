{-# LANGUAGE OverloadedStrings, LambdaCase #-}

import ConeServer.Types
import ConeServer.ConeTypes
import Parser.CSV
import IconGuesser
import ConeDemo

import qualified Data.Text              as T
import qualified Data.HashMap.Lazy      as M
import qualified Data.ByteString.Lazy   as B (writeFile)
import qualified Data.ByteString.Lazy.Char8   as B (pack)


import Data.Aeson.Encode.Pretty         (encodePretty)
import Data.Aeson                       (decode')
import System.FilePath.Posix            (takeFileName, replaceExtension, dropExtension)
import System.Environment
import Data.List
import Data.Ord
import Data.Maybe
import Control.Monad
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
    csv = filter (\row -> (not . null) row && row /= [""]) $ case header of
      (_:"arent"):(_:"ame"):_   -> trace ("dropped header: " ++ intercalate "," header) $ tail csv_
      _                         -> csv_

    parents :: M.HashMap T.Text Int
    parents = M.fromList . flip zip [0..] . map (T.pack . (!! 0)) $ csv

    grouped = groupByKey rows
    rows    = flip map csv $ \case
        []  -> error "empty row"
        [_] -> error "not enough columns"
        [par, name] ->
            (parents M.! T.pack par, (T.pack name, Nothing, Nothing))
        [par, name, uri] ->
            (parents M.! T.pack par, (T.pack name, checkUri uri, Nothing))
        par:name:uri:col:_ ->
            (parents M.! T.pack par, (T.pack name, checkUri uri, checkCol col))

    checkUri uri = if null uri then Nothing else Just $ T.pack uri
    checkCol []  = Nothing
    checkCol col = if head col == '#' && length col == 7 then Just $ B.pack col else Nothing

    attach e @ ConeEntry {ceLabel = label} =
        RoseLeaf e {ceIsLeaf = null children} (-1) $
            map (attach . entryWithUri) children
      where
        children = fromMaybe [] (M.lookup label parents >>= flip lookup grouped)

    entryWithUri (label, uri, col) = (entry label) {ceTargetUri = uri, ceColor = col >>= decode'}


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
        tree    = -- applyIconGuesser guesser $
                    buildFromCSV fName csv
        demo    = (fromConeTree tree) -- {subColorate = mkSubcolorate recruitColorization}
    in B.writeFile fName' (encodePretty tree)
        -- B.writeFile fName' (encodePretty demo)


{-
recruitColorization :: [(Text, DemoColorParams)]
recruitColorization =
    [ ("Housing", (Just ["#87c861"], Nothing))
    , ("Bridal", (Just ["#e93d9e"], Nothing))
    , ("Human Resources", (Just ["#f07942"], Nothing))
    , ("IT &Trends Media", (Just ["#3e91cd"], Nothing))
    , ("Education", (Just ["#3cb3a1"], Nothing))
    , ("Automobiles", (Just ["#f4a53b"], Nothing))
    , ("Travel", (Just ["#38b4ec"], Nothing))
    , ("Dining", (Just ["#ebc739"], Nothing))
    , ("Beauty", (Just ["#a749a1"], Nothing))
    , ("Life & Local Info", (Just ["#ed5148"], Nothing))
    ]
-}
