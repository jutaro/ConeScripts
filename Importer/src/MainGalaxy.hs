{-# LANGUAGE OverloadedStrings #-}

import GalaxyZoo
import ConeDemo

import ConeServer.ConeTypes
import ConeServer.Types

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode.Pretty         (encodePretty)
import qualified Data.ByteString.Lazy   as B (writeFile)
import Data.Text                        (Text, append)
import Debug.Trace



coneNodes :: [ConeTree] -> [ConeTree]
coneNodes = map (\(RoseLeaf a b cs) -> RoseLeaf a {ceIsLeaf = False} b cs)

-- MAIN

main :: IO ()
main = do
    glxRoot <- galaxyTree
    let
        glxZoo = ConeDemo "galaxies"
            Nothing
            Nothing
            False
            id
            (applyColorSerialization ColAsWebcolor glxRoot)
    B.writeFile "testData/scene4_1.json" $ encodePretty glxZoo
    


