{-# LANGUAGE OverloadedStrings, LambdaCase#-}

import ConeServer.Types
import ConeServer.ConeTypes
import IconGuesser
import ConeDemo

import Data.Maybe
import Control.Monad
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


keepTexts :: [Text]
keepTexts = ["title", "h2", "h1"]


-- TODO reveal.js targetUri scheme still needs correction

tagTreeToConeTree :: (Text, [TagTree Text]) -> ConeTree
tagTreeToConeTree (slidePath, ts) =
    RoseLeaf (slide {ceIsLeaf = null children}) (-1) children
  where
    root  = slidePath == "#"
    slide = (entry title) {ceTargetUri = if root || slidePath == "#/0" then Nothing else Just slidePath}
    title = fromMaybe T.empty $ do
        ~(TagBranch _ _ cs) <- mText
        findTextLeaf cs
    mText = listToMaybe $ concatMap (flip filterBranch ts . Just) keepTexts
    paths = map ((slidePath `T.append`) . ('/' `T.cons`) . T.pack . show) [0..]
    children =
        map (\(path, TagBranch _ _ bs) -> tagTreeToConeTree (path, bs))
            $ zip (if root then paths else tail paths)
            $ filterBranch (Just "section") ts

findTextLeaf :: [TagTree Text] -> Maybe Text
findTextLeaf [] = Nothing
findTextLeaf (x:xs) = case x of
    TagLeaf (TagText t) -> Just t
    _                   -> findTextLeaf xs

filterBranch :: Maybe Text -> [TagTree Text] -> [TagTree Text]
filterBranch Nothing = filter $ \case {TagBranch {} -> True; _ -> False}
filterBranch (Just s) = filter $ \case
    TagBranch a _ _ | a == s    -> True
    _                           -> False

cropBranch :: TagTree Text -> TagTree Text
cropBranch (TagBranch a _ cs)
    | a `elem` keepTexts    = TagBranch a [] cs
    | otherwise             = TagBranch a [] (map cropBranch $ filterBranch Nothing cs)
cropBranch a = a

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
            guesser <- newIconGuesser mIconPath
            T.readFile (last args) >>= main' guesser
  where
    usage = "usage: RevealImporter [-i <icon directory>] <reveal_index.html>"

main' :: IconGuesser -> Text -> IO ()
main' guesser inp =
    let
        tags    = filter tagFilter $ parseTags inp
        trees   = map cropBranch $ filterBranch Nothing $ tagTree tags
        tree    = applyIconGuesser guesser $ tagTreeToConeTree ("#", trees)
        json    = encodePretty $ (fromConeTree tree) {withNodeIds = True}
    in do
        putStrLn "---> found relevant subtrees:"
        mapM_ print trees
        B.writeFile "testData/revealjs.json" json
  where
    keep = "section" : keepTexts

    tagFilter (TagOpen t _) | t `elem` keep = True
    tagFilter (TagClose t)  | t `elem` keep = True
    tagFilter TagText {}                    = True
    tagFilter _                             = False
