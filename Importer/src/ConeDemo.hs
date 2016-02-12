{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module ConeDemo (ConeDemo(..), fromConeTree, entry, node, leaves) where

import ConeServer.ConeTypes
import ConeServer.Types
import Data.Text                        (Text, append)
import qualified Data.HashMap.Lazy      as HM (union, delete, map)
import qualified Data.Vector            as V (map)
import Data.Aeson                       (ToJSON(..), Value(..), (.=), object)


data ConeDemo = ConeDemo
    { basePrefix        :: String
    , baseColorsWeb     :: Maybe [String]
    , baseModifiers     :: Maybe [[Float]]
    , withNodeIds       :: Bool
    , theTree           :: ConeTree
    }

fromConeTree :: ConeTree -> ConeDemo
fromConeTree = ConeDemo "prefix" Nothing Nothing False

instance ToJSON ConeDemo where
    toJSON ConeDemo {..} =
        let
            ~(Object o1) = cleanup withNodeIds $ toJSON $
                if withNodeIds then enumerateTree coneEntrySetId 1 theTree else theTree
            ~(Object o2) = object
                [ "basePrefix"          .= basePrefix
                , "baseColors"          .= baseColorsWeb
                , "baseColorModifiers"  .= baseModifiers
                ]
        in Object $ o2 `HM.union` o1


-- HELPERS

cleanup :: Bool -> Value -> Value
cleanup keepNodeId = \case
    Array arr   -> Array $ V.map (cleanup keepNodeId) arr
    Object o    -> Object $ HM.map (cleanup keepNodeId) $ foldr HM.delete o killAttribs
    v           -> v
  where
    killAttribs =
        let attrs = ["nodeId", "tag"]
        in if keepNodeId then tail attrs else attrs


entry :: Text -> ConeEntry
entry t = emptyLeaf {ceLabel = t, ceTextId = "tId_" `append` t}


node :: ConeEntry -> [ConeTree] -> ConeTree
node e [] = RoseLeaf e {ceIsLeaf = ceIsLeaf e && True} (-1) []
node e cs = RoseLeaf e {ceIsLeaf = False} (-1) cs


leaves :: Bool -> [ConeEntry] -> [ConeTree]
leaves asCones = map (flip node [] . (\e -> e {ceIsLeaf = not asCones}))


{-
injectAttribs :: Text -> Value -> Value
injectAttribs key (Array arr) = Array $ V.map (injectAttribs key) arr
-- TODO inject attrib on certain condition (e.g. tId selection)
injectAttribs _ v = v
-}
