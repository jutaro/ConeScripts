{-# LANGUAGE OverloadedStrings #-}

module  IconGuesser
        ( IconGuesser
        , newIconGuesser
        , applyIconGuesser
        ) where

import  ConeServer.ConeTypes
import  GHC.Exts                    (Down(..))

import  qualified Data.Text         as T
import  System.Directory
import  Control.Monad               (forM)
import  Control.Arrow               (first)
import  Control.Applicative         ((<|>))
import  Data.Char                   (isAlpha, isPunctuation)
import  Data.List                   (maximumBy, isSuffixOf, intersect, sortBy)
import  Data.Ord                    (comparing)

import  Debug.Trace

type    TagSet      = [T.Text]
type    TagDict     = [(TagSet, T.Text)]
newtype IconGuesser = IconGuesser {guessIcon :: ConeEntry -> Maybe T.Text}

iconGuessId :: IconGuesser
iconGuessId = IconGuesser ceIconName

iconGuessDefault :: IconGuesser
iconGuessDefault = IconGuesser $ \ConeEntry {ceIsLeaf = isLeaf} ->
    if isLeaf
        then Just "propertyDefault.png"                                         -- TODO
        else Just "classDefault.png"                                            -- TODO

iconGuessByTags :: TagDict -> IconGuesser
iconGuessByTags dict =
    IconGuesser guessFunc
  where
    guessFunc e @ ConeEntry {ceLabel = label} =
        case matches' of
            (_:_, icon):_   -> Just icon                                        -- non-empty set of intersecting tags, sorted by cardinality
            _               -> Nothing
      where
        matches     = map (first (extractTags label `intersect`)) dict
        matches'    = sortBy (comparing (Down . length . fst)) matches


newIconGuesser :: Maybe FilePath -> IO IconGuesser
newIconGuesser Nothing = do
    traceIO "Icon auto-guess step will be skipped"
    return iconGuessId
newIconGuesser (Just iconDir) = do
    dirExists <- doesDirectoryExist iconDir
    if dirExists
        then do
            tagDict <- loadRecursive' iconDir
            -- traceIO $ show tagDict
            traceIO $ "Icons tagged: " ++ show (length tagDict)
            return  $ iconGuessByTags tagDict
        else do
            traceIO "Error: specified icon directory not found"
            newIconGuesser Nothing


applyIconGuesser :: IconGuesser -> ConeTree -> ConeTree
applyIconGuesser IconGuesser {guessIcon = guessIcon} =
    fmap modifyEntry
  where
    modifyEntry e = e {ceIconName = ceIconName e <|> guessIcon e}


loadRecursive' :: FilePath -> IO TagDict
loadRecursive' dir = do
    dir_ <- getCurrentDirectory
    setCurrentDirectory dir
    files <- filter (`notElem` [".", ".."]) <$> getDirectoryContents "."
    xs <- forM files $ \file -> do
        isDir <- doesDirectoryExist file
        if isDir then loadRecursive' file else let f' = T.pack file in return
            [(extractTags $ dropSuff f', f') | any (`isSuffixOf` file) extensions]
    setCurrentDirectory dir_
    length xs `seq` return (concat xs)
  where
    extensions  = [".png", ".PNG", ".jpg", ".JPG"]
    dropSuff    = T.dropEnd 4


extractTags :: T.Text -> TagSet
extractTags fName =
    filter (\t -> T.length t > 2 && t `notElem` stopWords)
    . map (T.toLower . T.filter isAlpha)
    . concatMap (T.split isPunctuation)
    $ longestSplit
  where
    longestSplit = maximumBy (comparing length)
        . map (`T.splitOn` fName)
        $ delims
    delims      = ["-", " ", "_"]
    stopWords   = ["icon", "the", "glossy", "silver"]
