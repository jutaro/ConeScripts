{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module ConeDemo
    ( ConeDemo(..)
    , DemoColorParams
    , fromConeTree
    , explodeCones
    , entry
    , node
    , leaves
    , modify
    , hasKeyValue
    , mkSubcolorate
    ) where

import ConeServer.ConeTypes
import ConeServer.Types
import Data.Text                        (Text, append)
import qualified Data.HashMap.Lazy      as HM (union, delete, map, lookup, insert)
import qualified Data.Vector            as V (map)
import Data.Aeson                       (ToJSON(..), Value(..), (.=), object)


type DemoColor          = Maybe [String]
type DemoColorMod       = Maybe [[Float]]
type DemoColorParams    = (DemoColor, DemoColorMod)

data ConeDemo = ConeDemo
    { basePrefix        :: String
    , baseColorsWeb     :: DemoColor
    , baseModifiers     :: DemoColorMod
    , withNodeIds       :: Bool
    , subColorate       :: Value -> Value
    , theTree           :: ConeTree
    }

fromConeTree :: ConeTree -> ConeDemo
fromConeTree = ConeDemo "prefix" Nothing Nothing False id

-- all cones are displayed as cones; no leaf nodes left in a tree
explodeCones :: ConeTree -> ConeTree
explodeCones = fmap explodeEntry
  where
    explodeEntry e = e {ceIsLeaf = False}

instance ToJSON ConeDemo where
    toJSON ConeDemo {..} =
        let
            ~(Object o1)
                = subColorate
                $ cleanup withNodeIds
                $ toJSON
                $ if withNodeIds then enumerateTree coneEntrySetId 1 theTree else theTree
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


modify :: (Value -> Value) -> (Value -> Bool) -> Value -> Value
modify f cond =
    modify'
  where
    modify' v@(Array arr) =
        let v' = Array $ V.map modify' arr
        in if cond v then f v' else v'
    modify' v@(Object o) =
        let v' = Object $ HM.map modify' o
        in if cond v then f v' else v'
    modify' v = if cond v then f v else v

hasKeyValue :: Text -> Text -> Value -> Bool
hasKeyValue k v_ = \case
    Object o    -> HM.lookup k o == Just (String v_)
    _           -> False

injectKeyValue :: Text -> Value -> Value -> Value
injectKeyValue k v_ = \case
    Object o    -> Object $ HM.insert k v_ o
    v           -> v

injectColoration :: DemoColorParams -> Text -> Value -> Value
injectColoration (Nothing, Nothing) _ = id
injectColoration (cols, mods) tId =
    modify colorize (hasKeyValue "textId" $ "tId_" `append` tId)
  where
    colorize
        = injectKeyValue "baseColors" (toJSON cols)
        . injectKeyValue "baseColorModifiers" (toJSON mods)


mkSubcolorate :: [(Text, DemoColorParams)] -> Value -> Value
mkSubcolorate cs v = foldr (\(tId, c) v' -> injectColoration c tId v') v cs
