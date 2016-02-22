{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module GalaxyZoo (galaxyTree) where
 
import ConeDemo  
import ConeServer.Types
import ConeServer.ConeTypes

import Data.Aeson (decode')
import Data.Text as T (Text, append, empty)
import Data.ByteString.Lazy as BL (ByteString)
import Test.QuickCheck
        (Arbitrary(..), Gen, generate, choose, resize, sized, elements,
        listOf1, suchThat)


galaxyTree :: IO ConeTree
galaxyTree = do
    home <- milkyWay
    others <- mapM (generate . genGalaxy) galaxies
    return $ RoseLeaf emptyLeaf {ceIsLeaf = False} (-1) (map toTree $ home:others)
  where
    genGalaxy n = do
        Galaxy (_, cs) <- arbitrary
        return $ Galaxy (Body n Nothing, cs)

data Body = Body
    { name  :: Text
    , color :: Maybe BL.ByteString
    }

toEntry :: Body -> ConeEntry
toEntry Body {..} = emptyLeaf 
    { ceLabel   = name
    , ceTextId  = "tId_" <> name
    , ceColor   = color >>= decode'
    }

class ToTree a where
    toTree :: a -> ConeTree

newtype Galaxy  = Galaxy    (Body, [Arm])
newtype Arm     = Arm       (Body, [Object])  -- ~30 objects?
newtype Object  = Object    (Body, [Star])    -- cluster or nebula, many stars
newtype Star    = Star      (Body, [Planet])
newtype Planet  = Planet    (Body, [Moon])
type    Moon    = Body


instance ToTree Galaxy where
    toTree (Galaxy (e, cs)) =
        let ds = map toTree cs
        in RoseLeaf ((toEntry e) {ceIsLeaf = null ds}) (-1) ds

instance ToTree Arm where
    toTree (Arm (e, cs)) =
        let ds = map toTree cs
        in RoseLeaf ((toEntry e) {ceIsLeaf = null ds}) (-1) ds
        
instance ToTree Object where
    toTree (Object (e, cs)) =
        let ds = map toTree cs
        in RoseLeaf ((toEntry e) {ceIsLeaf = null ds}) (-1) ds
        
instance ToTree Star where
    toTree (Star (e, cs)) =
        let ds = map toTree cs
        in RoseLeaf ((toEntry e) {ceIsLeaf = null ds}) (-1) ds        

instance ToTree Planet where
    toTree (Planet (e, cs)) =
        let ds = leaves True $ map toEntry cs
        in RoseLeaf ((toEntry e) {ceIsLeaf = null ds}) (-1) ds

moon = Body "Moon" Nothing

instance Arbitrary Galaxy where
    arbitrary = do
        a       <- choose (2 :: Int, 7)
        -- arms    <- replicateM a arbitrary
        return  $ Galaxy (Body empty Nothing, []) 

galaxies :: [Text]
galaxies = 
    [ "Andromeda - M31" -- 7 arms
    , "Black Eye Galaxy"
    , "Bode's - M81"
    , "Cartwheel Galaxy"
    , "Cigar Galaxy"
    , "Comet Galaxy"
    , "Cosmos Redshift 7"
    , "Hoag's Object"
    , "Large Magellanic Cloud"
    , "Small Magellanic Cloud"
    , "Mayall's Object"
    , "Pinwheel - M101"
    , "Sombrero - M104"
    , "Sunflower Galaxy"
    , "Tadpole Galaxy"
    , "Triangulum - M33"
    , "Whirlpool Galaxy"
    ]
    
milkyWay :: IO Galaxy 
milkyWay = do 
    o <- orionCygnus
    return $ Galaxy 
        ( Body "Milky Way" Nothing
        , o : [] -- map (\a -> )
        ) 
  where
    arms :: [Text]
    arms =
        [ "Perseus"
        , "Outer Arm"
        , "Scutum–Centaurus"
        , "Carina–Sagittarius"
        ]

orionCygnus :: IO Arm
orionCygnus = return $ Arm
        ( Body "Orion–Cygnus" Nothing
        , []
        )
