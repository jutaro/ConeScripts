
-----------------------------------------------------------------------------
--
-- Module      :  CSV
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Script for importing rdf files in N-Triples, Turtle and RDF/XML
-- and exporting as ConeServer model file
--
-----------------------------------------------------------------------------

module CSV (
    processCSV
) where

import Parser.CSV
import ConeServer.Types
import ConeServer.ConeTypes
import Data.Text (pack, unpack, Text)
import Text.ParserCombinators.Parsec

getGroups :: Record -> String
getGroups record = record !! 87

processCSV :: CSV -> ConeTree
processCSV csv =
    let personCones = mapMaybe makeEntryCone csv
        baseTree    = RoseLeaf
                        { roseLeaf      = addressEntry
                        , roseMeta      = 0
                        , roseChildren  = []
                        }
    in foldr addToGroups baseTree personCones
  where addressEntry = ConeEntry {
                        ceEntryId     = 0
                      , ceLabel       = pack "Contacts"
                      , ceTextId       = pack "Contacts"
                      , ceTargetUri   = Nothing
                      , ceComment     = Nothing
                      , ceIconName    = Nothing
                      , ceStlName     = Nothing
                      , ceColor       = Nothing
                      , ceIsLeaf      = False
                      }

addToGroups :: ConeTree -> ConeTree -> ConeTree
addToGroups personCone tree =
    let groupsString = unpack $ fromMaybe (pack "") (ceComment (roseLeaf personCone))
        groups = pack "All" : (case parse parseGroupsString "" groupsString of
                            Left err -> trace "failedToParseGroups" []
                            Right res -> trace ("trace groupsString:" ++ groupsString ++ " parsed as: " ++ show res)
                                                    $ map pack res)
        groups' = filter (\t -> t /= pack "") groups
    in foldr (addGroup personCone) tree groups'

addGroup ::  ConeTree -> Text -> ConeTree -> ConeTree
addGroup personCone group tree =
    case filter (\t -> ceLabel (roseLeaf t) == group) (roseChildren tree) of
        (subtree:_) ->
            let newSubtree = subtree{roseChildren = personCone : roseChildren subtree}
            in tree {roseChildren = newSubtree : filter (\t -> ceLabel (roseLeaf t) /= group) (roseChildren tree)}
        [] -> let newSubtree = RoseLeaf
                        { roseLeaf      = groupEntryFor group
                        , roseMeta      = 0
                        , roseChildren  = [personCone]
                        }
              in tree {roseChildren = newSubtree : roseChildren tree}

groupEntryFor :: Text -> ConeEntry
groupEntryFor name = ConeEntry {
                      ceEntryId     = 0
                    , ceLabel       = name
                    , ceTextId      = name
                    , ceTargetUri   = Nothing
                    , ceComment     = Nothing
                    , ceIconName    = Nothing
                    , ceStlName     = Nothing
                    , ceColor       = Nothing
                    , ceIsLeaf      = False
                    }

parseGroupsString :: Parser [String]
parseGroupsString = do
    r <- many (noneOf ";") `sepBy` string ";"
    return (map trim r)

-- | Auxiliary string functions. I can't believe no module declares these
trim :: String -> String
trim = trimEnd . trimBeginning

trimBeginning :: String -> String
trimBeginning = dropWhile (== ' ')

trimEnd :: String -> String
trimEnd = reverse . trimBeginning . reverse

makeEntryCone :: Record -> Maybe ConeTree
makeEntryCone record =
    case head record of
        "" -> Nothing
        name -> Just RoseLeaf
                { roseLeaf      = entry name record
                , roseMeta      = 0
                , roseChildren  = childLeafs name record
                }
  where
    entry name record = ConeEntry {
                          ceEntryId     = 0
                        , ceLabel       = pack name
                        , ceTextId      = pack name
                        , ceTargetUri   = Nothing
                        , ceComment     = Just (pack (getGroups record))
                        , ceIconName    = Nothing
                        , ceStlName     = Nothing
                        , ceColor       = Nothing
                        , ceIsLeaf      = False
                        }
    childLeafs name record = []
