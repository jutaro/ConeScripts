{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import IconGuesser
import PeopleZoo

import ConeServer.ConeTypes
import ConeServer.Types

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode.Pretty         (encodePretty)
import qualified Data.ByteString.Lazy   as B (writeFile)
import qualified Data.HashMap.Lazy      as HM (union, delete, map)
import qualified Data.Vector            as V (map)
import Data.Text                        (Text, append)
import System.Random                    (randomRIO)
import System.Environment
import Debug.Trace


data ConeDemo = ConeDemo
    { basePrefix        :: String
    , baseColorsWeb     :: Maybe [String]
    , baseModifiers     :: Maybe [[Float]]
    , theTree           :: ConeTree
    }


instance ToJSON ConeDemo where
    toJSON ConeDemo {..} =
        let
            ~(Object o1) = cleanup $ toJSON theTree
            ~(Object o2) = object
                [ "basePrefix"          .= basePrefix
                , "baseColors"          .= baseColorsWeb
                , "baseColorModifiers"  .= baseModifiers
                ]
        in Object $ o2 `HM.union` o1


-- HELPERS

cleanup :: Value -> Value
cleanup (Array arr) = Array $ V.map cleanup arr
cleanup (Object o)  = Object $ HM.map cleanup $ foldr HM.delete o ["tag", "nodeId"]
cleanup v           = v

injectAttribs :: Text -> Value -> Value
injectAttribs key (Array arr) = Array $ V.map (injectAttribs key) arr
injectAttribs _ v = v

entry :: Text -> ConeEntry
entry t = emptyLeaf {ceLabel = t, ceTextId = "tId_" `append` t}

leaves :: Bool -> [ConeEntry] -> [ConeTree]
leaves asCones = map (flip node [] . (\e -> e {ceIsLeaf = not asCones}))

node :: ConeEntry -> [ConeTree] -> ConeTree
node e [] = RoseLeaf e {ceIsLeaf = ceIsLeaf e && True} (-1) []
node e cs = RoseLeaf e {ceIsLeaf = False} (-1) cs

coneNodes :: [ConeTree] -> [ConeTree]
coneNodes = map (\(RoseLeaf a b cs) -> RoseLeaf a {ceIsLeaf = False} b cs)

-- MAIN

main :: IO ()
main = do
    args <- getArgs
    let
        mIconPath = case args of
            ["-i", iconDir] -> Just iconDir
            _               -> Nothing
    iconGuesser <- newIconGuesser mIconPath

    let
        coneDemo = ConeDemo "base"
            (Just ["#273f61", "#325765", "#4d818c"])
            -- (Just [[0.95, 0.95, 0.95], [0.85, 0.85, 0.85], [0.8, 0.8, 0.8]])
            Nothing
            (applyIconGuesser iconGuesser root1)

    B.writeFile "scene3_1.json" $ encodePretty $ coneDemo

    pplRoot <- buildRoot
    let
        pplZoo = ConeDemo "people"
            Nothing
            (Just [[0.96, 0.96, 0.96]])
            (applyColorSerialization ColAsWebcolor pplRoot)
    B.writeFile "scene3_2.json" $ encodePretty $ pplZoo


--
-- Scene3_1
--

root1 :: ConeTree
root1 = node (entry "Business Needs") [legal, financial, infrastructure]


legal = node (entry "Legal") $ leaves True $ map entry
    ["Health & Safety", "Accounting", "Human Resources", "Insurance"]


financial = node (entry "Financial") [prod, sales, management]

management = node (entry "Management") $ coneNodes
    [ productivity
    , facilities
    , location
    , licensing
    , software
    , humRes
    ]
facilities  = node (entry "Facilities") []
location    = node (entry "Location") []
licensing   = node (entry "Licensing") []
software    = node (entry "Software") []
humRes      = node (entry "Human Resources") hrChildren
  where
    hrChildren = leaves True . map entry $
        ["Training", "Recruitment", "Communication"]
productivity = node (entry "Productivity") prodChildren
  where
    prodChildren = leaves True . map entry $
        ["Dashboards", "Analytics", "Reporting", "Trends", "BPM", "Operations", "Logistics"]


sales = node (entry "Sales") $ leaves True $ map entry
    ["Distribution", "Processing", "Customer Service", "Selling", "Marketing", "Market Research"]

prod = node (entry "Product") $ leaves True $ map entry
    ["Skills", "Staff", "Raw Materials", "Suppliers", "Innovation", "Potential", "Competitiveness"]

infrastructure = node (entry "Infrastructure") []




--
-- Scene3_2
--

buildRoot :: IO ConeTree
buildRoot = do
    [dep1, dep2, dep3, dep4] <- leaves True <$> replicateM 4 (spawnPerson 1)
    dep1'   <- (node (entry "IT") . (:[]))          <$> populateZoo 1 [(4,5), (7,10)] dep1
    dep2'   <- (node (entry "Management") . (:[]))  <$> populateZoo 1 [(2,3), (4,5)] dep2
    dep3'   <- (node (entry "Marketing") . (:[]))   <$> populateZoo 1 [(2,4), (12,14)] dep3
    dep4'   <- (node (entry "Financial") . (:[]))   <$> populateZoo 1 [(3,4), (5,8)] dep4
    return  $ node (entry "Deparments") [dep1', dep2', dep3', dep4']


populateZoo :: Int -> [(Int, Int)] -> ConeTree -> IO ConeTree
populateZoo level (lim:lims) (RoseLeaf e x _) = do
    count   <- randomRIO lim
    ppl     <- leaves True <$> replicateM count (spawnPerson level)
    cs      <- if null lims then return ppl else mapM (populateZoo (level-1) lims) ppl
    return  $ RoseLeaf e {ceIsLeaf = False} x cs
