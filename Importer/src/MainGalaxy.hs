{-# LANGUAGE OverloadedStrings #-}

import GalaxyZoo
import ConeDemo

import ConeServer.ConeTypes
import ConeServer.Types

import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode.Pretty         (encodePretty)
import qualified Data.ByteString.Lazy   as B (writeFile)
import Data.Text                        (Text, append, singleton)
import Debug.Trace



coneNodes :: [ConeTree] -> [ConeTree]
coneNodes = map (\(RoseLeaf a b cs) -> RoseLeaf a {ceIsLeaf = False} b cs)

-- MAIN

main :: IO ()
main = do
    glxRoot <- galaxyTree
    let
        root = node ((entry "") {ceColor = decode' "\"#83b4c7\""}) (glxRoot : leaves True firstLevel)

        glxZoo = ConeDemo "galaxies"
            Nothing
            Nothing
            False
            id
            (applyColorSerialization ColAsWebcolor root)
    B.writeFile "testData/scene4_1.json" $ encodePretty glxZoo




firstLevel :: [ConeEntry]
firstLevel =
    [(entry "") {ceColor = Just col, ceTextId = "tId_" `append` singleton tid}
        | (col, tid) <- zip firstLevelCols ['a'..]]

firstLevelCols :: [ConeColor]
firstLevelCols =
    mapMaybe dec ["#b6829b", "#c2a8b6", "#907c83", "#71616d", "#774e5f", "#885469"]
  where
    dec col = decode' $ "\"" <> col <> "\""
