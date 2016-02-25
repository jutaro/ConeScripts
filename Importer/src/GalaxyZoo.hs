{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module GalaxyZoo (galaxyTree) where
 
import ConeDemo  
import ConeServer.Types
import ConeServer.ConeTypes

import Data.Aeson (decode')
import Data.Text as T (Text, append, empty, pack)
import Data.ByteString.Lazy as BL (ByteString)
import Test.QuickCheck
        (Arbitrary(..), Gen, generate, choose, resize, sized, elements,
        listOf1, suchThat)


galaxyTree :: IO ConeTree
galaxyTree = do    
    home    <- milkyWay
    others  <- mapM (generate . genGalaxy) galaxies
    return  $ RoseLeaf emptyLeaf {ceIsLeaf = False} (-1) (map toTree $ home:others)
  where
    genGalaxy n = do
        Galaxy (Body _ col, cs) <- arbitrary
        return $ Galaxy (Body n col, cs)

data Body = Body
    { name  :: Text
    , color :: Maybe BL.ByteString
    }

setName :: Text -> Body -> Body
setName n' b = b {name = n'}

toEntry :: Body -> ConeEntry
toEntry Body {..} = emptyLeaf 
    { ceLabel   = name
    , ceTextId  = "tId_" <> name
    , ceColor   = do
        c <- color
        decode' $ "\"" <> c <> "\""
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
        in RoseLeaf ((toEntry e) {ceIsLeaf = null ds, ceIconName = Just "galaxy.png"}) (-1) ds

instance ToTree Arm where
    toTree (Arm (e, cs)) =
        let ds = map toTree cs
        in RoseLeaf ((toEntry e) {ceIsLeaf = null ds, ceIconName = Just "galaxy.png"}) (-1) ds
        
instance ToTree Object where
    toTree (Object (e, cs)) =
        let ds = map toTree cs
        in RoseLeaf ((toEntry e) {ceIsLeaf = null ds, ceIconName = Just "object_nebula.png"}) (-1) ds
        
instance ToTree Star where
    toTree (Star (e, cs)) =
        let ds = map toTree cs
        in RoseLeaf ((toEntry e) {ceIsLeaf = null ds, ceIconName = Just "star.png"}) (-1) ds        

instance ToTree Planet where
    toTree (Planet (e@(Body pName _), cs)) =
        let 
            ds = leaves True $ map ((\e -> e {ceIconName = Just "moon.png"}) . toEntry) cs
            icon = if pName == "Earth" then "earth.png" else "planet.png"
        in RoseLeaf ((toEntry e) {ceIsLeaf = False, ceIconName = Just icon}) (-1) ds


instance Arbitrary Galaxy where
    arbitrary = do
        a       <- choose (2 :: Int, 8)
        arms    <- replicateM a arbitrary
        let
            ns  = [T.pack n' | n <- [1..a], let n' = show n]
            xs  = zipWith T.append (map (\(Arm (Body n _, _)) -> n) arms) ns
            os  = zipWith (\n' (Arm (b, bs)) -> Arm (setName n' b, bs)) xs arms 
        return  $ Galaxy (Body empty glxCol, os) 
        
instance Arbitrary Arm where
    arbitrary = do
        a       <- elements [15 :: Int, 15, 25, 30, 30, 30, 45]
        ns      <- elements ["N", "S"]
        objs    <- replicateM a arbitrary
        return  $ Arm (Body ns armCol, objs)
    
instance Arbitrary Object where
    arbitrary = do
        a       <- choose (1 :: Int, 4)
        stars   <- replicateM a (resize 0 arbitrary)
        n       <- choose (1 :: Int, 100) 
        cn      <- elements ["Cluster", "Nebula", "Inner Cluster", "Outer Nebula", "Belt"]
        let nm  = T.pack $ cn ++ " M" ++ show n 
        return  $ Object (Body nm objCol, stars)

instance Arbitrary Star where
    arbitrary = sized $ \size -> do
        n       <- choose (200 :: Int, 900)
        let nm  = T.pack $ "M" ++ show n
        cs <- if size == 0
            then return [Planet (Body "" Nothing, [])]
            else return [] 
        return  $ Star (Body nm starCol, cs)
        

starCol :: Maybe BL.ByteString
starCol = Just "#67c3d2"

objCol :: Maybe BL.ByteString
objCol = Just "#4a8b95"

armCol :: Maybe BL.ByteString
armCol = Just "#375f78"

glxCol :: Maybe BL.ByteString
glxCol = Just "#2e7eb0"

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
    home    <- orionCygnus
    others  <- mapM (generate . genArm) arms
    return  $ Galaxy (Body "Milky Way" glxCol, home:others) 
  where
    genArm n = do
        Arm (_, cs) <- arbitrary
        return $ Arm (Body n armCol, cs)
    arms :: [Text]
    arms =
        [ "Perseus"
        , "Outer Arm"
        , "Scutum–Centaurus"
        , "Carina–Sagittarius"
        ]

orionCygnus :: IO Arm
orionCygnus = do
    home    <- gouldBelt
    others  <- replicateM 29 (generate arbitrary)
    return  $ Arm (Body "Orion–Cygnus" armCol, home:others)
  

gouldBelt :: IO Object
gouldBelt = do
    -- others <- replicateM 15 (generate arbi)
    return  $ Object (Body "Gould Belt" objCol, [solSystem])

moon :: Text -> Moon
moon = flip Body (Just "#636363")

solSystem :: Star
solSystem = Star (Body "Sol" starCol,
    [ Planet (Body "Mercury" (Just "#8a6f5c"), [])
    , Planet (Body "Venus" (Just "#e1a253"), [])
    , Planet (Body "Earth" (Just "#1d46a4"), [moon "Luna"])
    , Planet (Body "Mars" (Just "#69291f"), [moon "Phobos", moon "Deimos"])
    , Planet (Body "Jupiter" (Just "#b18868"), map moon ["Io", "Ganymede", "Europa", "Callisto"])
    , Planet (Body "Saturn" (Just "#e0d65a"), map moon ["Mimas", "Enceladus", "Tethys", "Dione", "Rhea", "Titan", "Iapetus"])
    , Planet (Body "Uranus" (Just "#60b3bb"), map moon ["Titania", "Oberon", "Umbriel", "Ariel", "Miranda", "Sycorax"])
    , Planet (Body "Neptune" (Just "#84a3e6"), map moon ["Triton", "Proteus", "Nereis", "Larissa", "Galatea"])
    ])
