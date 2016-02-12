{-# LANGUAGE OverloadedStrings, LambdaCase #-}

import ConeServer.Types
import ConeServer.ConeTypes
import IconGuesser
import ConeDemo

import qualified Data.ByteString.Lazy   as B (writeFile)
import Data.Aeson.Encode.Pretty         (encodePretty)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.StringLike

import System.Environment               (getArgs)
import Data.List
import Debug.Trace


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
            guesser <- newIconGuesser mIconPath
            readFile (last args) >>= main' guesser
            {-
            let fName = last args
            guesser <-
            t
            either (putStrLn . show) (main' fName guesser) eCsv -}
  where
    usage = "usage: RevealImporter [-i <icon directory>] reveal_index.html"



main' :: IconGuesser -> String -> IO ()
main' guesser inp =
    let
        tags    = parseTags inp
        ttree   = tagTree $ filter tagFilter tags
        tree    = applyIconGuesser guesser emptyTree
    in do
        putStrLn "tags:"
        mapM_ print tags
        putStrLn "tree:"
        print ttree
        B.writeFile "testData/revealjs.json" (encodePretty $ fromConeTree tree)
  where
    tagFilter (TagOpen "section" _) = True
    tagFilter (TagClose "section")  = True
    tagFilter _ = False
