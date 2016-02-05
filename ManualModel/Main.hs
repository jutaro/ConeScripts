{-# LANGUAGE OverloadedStrings #-}

import IconGuesser

import ConeServer.ConeTypes
import ConeServer.Types

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode.Pretty         (encodePretty)
import qualified Data.ByteString.Lazy   as B (writeFile)
import qualified Data.HashMap.Lazy      as HM (union, delete, map)
import qualified Data.Vector            as V (map)
import Data.Text                        (Text, append)
import System.Environment
import Debug.Trace


data ConeDemo = ConeDemo
    { basePrefix        :: String
    , baseColorsWeb     :: Maybe [String]
    , theTree           :: ConeTree
    }


instance ToJSON ConeDemo where
    toJSON (ConeDemo basePref cols tree) =
        let
            ~(Object o1) = cleanup $ toJSON tree
            ~(Object o2) = object
                [ "basePrefix" .= basePref
                , "baseColors" .= cols
                ]
        in Object $ o1 `HM.union` o2


-- HELPERS

cleanup :: Value -> Value
cleanup (Array arr) = Array $ V.map cleanup arr
cleanup (Object o)  = Object $ HM.map cleanup $ foldr HM.delete o ["tag", "nodeId"]
cleanup v           = v

entry :: Text -> ConeEntry
entry t = emptyLeaf {ceLabel = t, ceTextId = "tId_" `append` t}

leaves :: Bool -> [ConeEntry] -> [ConeTree]
leaves asCones = map (flip node [] . (\e -> e {ceIsLeaf = not asCones}))

node :: ConeEntry -> [ConeTree] -> ConeTree
node e [] = RoseLeaf e {ceIsLeaf = ceIsLeaf e && True} (-1) []
node e cs = RoseLeaf e {ceIsLeaf = False} (-1) cs



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
        coneDemo = ConeDemo
            "base"
            (Just ["#4532A2", "#222222"])
            (applyIconGuesser iconGuesser root)

    B.writeFile "exportModel.json" $ encodePretty $  coneDemo



-- MANUAL DEFINITION

root :: ConeTree
root = node (entry "Business Needs") [financial]

financial = node (entry "Financial") [management]

management = node (entry "Management") [productivity, facilities]

facilities = node (entry "Facilities") []

productivity = node (entry "Productivity") prodChildren
  where
    prodChildren = leaves True . map entry $
        ["Dashboards", "Analytics", "Reporting", "Trends", "BPM", "Operations", "Logistics"]
