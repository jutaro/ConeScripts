{-# LANGUAGE OverloadedStrings, LambdaCase#-}

import ConeServer.Types
import ConeServer.ConeTypes
import IconGuesser
import ConeDemo

import Data.Text                        (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.ByteString.Lazy   as B (writeFile)
import Data.Aeson.Encode.Pretty         (encodePretty)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.StringLike

import System.Environment               (getArgs)
import Data.List
import Data.String                      (IsString)
import Debug.Trace


tagTreeToConeTree :: [TagTree Text] -> ConeTree
tagTreeToConeTree ts =
    RoseLeaf ((entry title) {ceIsLeaf = null children}) (-1) children
  where
    title = fromMaybe T.empty $ do
        ~(TagBranch _ _ cs) <- mText
        findTextLeaf cs
    mText = listToMaybe $ concatMap (`filterBranch` ts) ["title", "h2", "h1"]
    -- TODO get targetUri
    children =
        map (\(TagBranch _ _ bs) -> tagTreeToConeTree bs)
            $ filterBranch "section" ts


treeBranch :: TagTree Text -> Bool
treeBranch = \case {TagBranch {} -> True; _ -> False}

findTextLeaf :: [TagTree Text] -> Maybe Text
findTextLeaf [] = Nothing
findTextLeaf (x:xs) = case x of
    TagLeaf (TagText t) -> Just t
    _                   -> findTextLeaf xs

filterBranch :: Text -> [TagTree Text] -> [TagTree Text]
filterBranch s = filter $ \case
    TagBranch a _ _ | a == s    -> True
    _                           -> False

cropBranch :: TagTree Text -> TagTree Text
cropBranch (TagBranch a _ cs)
    | a `elem` ["h1", "h2", "title"] = TagBranch a [] cs
    | otherwise                      = TagBranch a [] [cropBranch c | c <- cs, treeBranch c]
cropBranch a = a

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
            T.readFile (last args) >>= main' guesser
  where
    usage = "usage: RevealImporter [-i <icon directory>] <reveal_index.html>"

main' :: IconGuesser -> Text -> IO ()
main' guesser inp =
    let
        tags    = filter tagFilter $ parseTags inp
        section = [cropBranch tree | tree <- tagTree tags, treeBranch tree]
        tree    = applyIconGuesser guesser $ tagTreeToConeTree section
        json    = encodePretty $ fromConeTree tree
    in do
        putStrLn "trees:"
        mapM_ print section

        B.writeFile "testData/revealjs.json" json
  where
    keep = ["section", "h1", "h2", "title"]

    tagFilter (TagOpen t _) | t `elem` keep = True
    tagFilter (TagClose t)  | t `elem` keep = True
    tagFilter TagText {}                    = True
    tagFilter _                             = False
