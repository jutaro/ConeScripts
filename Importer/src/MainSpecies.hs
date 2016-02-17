-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Script for importing rdf files in N-Triple from geospecies database.
--
-----------------------------------------------------------------------------

import IconGuesser
import ConeDemo (fromConeTree)

import System.Console.GetOpt
import System.Environment
import Data.Version (showVersion)
import Paths_Importer
import System.FilePath.Posix(takeExtension, replaceExtension, dropExtension)
import System.Exit
import Data.RDF
import Debug.Trace
import Data.Text (pack, unpack, Text)
import qualified Data.ByteString.Lazy as BS(writeFile)

import ConeServer.Types
import ConeServer.ConeTypes
import Parser.CSV
import CSV

import Data.Aeson.Encode.Pretty (encodePretty)


data Flag =  LanguageCode String
             | RootId String
             | IconDir FilePath
             | CSV
             | Version
             | Help
       deriving (Show,Eq)

options :: [OptDescr Flag]
options =   [
            Option ['l'] ["lang"] (ReqArg LanguageCode "Language Code")
               "Pass the desired language code"
        ,   Option ['r'] ["root"] (ReqArg RootId "Root URI")
           "Pass the desired language code"
        ,   Option ['i'] ["icons"] (ReqArg IconDir "Icon directory")
              "Pass a directory containing icons / textures"
        ,   Option ['c'] ["csv"] (NoArg CSV)
              "Import from csv"
        ,   Option ['v'] ["version"] (NoArg Version)
               "Show the version number"
        ,   Option ['h'] ["help"] (NoArg Help)
               "Display command line options"
           ]

getOpts :: [String] -> IO ([Flag], [String])
getOpts argv =
   case getOpt Permute options argv of
         (o,n,[]  ) -> return (o,n)
         (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo header options

header :: String
header = "Usage: RDFImporter [OPTION...] fileName"

main = do
    args             <- getArgs
    (o,files)        <- getOpts args
    trace "123" $ return ()
    let condIconDir  =  foldr (\x y -> case x of IconDir d -> Just d; _ -> y) Nothing o
    let lang         =  case filter (\x -> case x of
                                        LanguageCode p -> True
                                        _           -> False) o of
                            []                -> "en"
                            LanguageCode p :_ -> p
    if Version `elem` o
        then putStrLn $ "RDFImporter, version " ++ showVersion version
        else if Help `elem` o
            then putStrLn $ usageInfo header options
            else if CSV `elem` o
                then case files of
                        (f : _) -> do
                                eitherErrCSV <- parseCSVFromFile f
                                case eitherErrCSV of
                                    Left parseError -> do
                                        putStrLn $ "Parse error: " ++ show parseError
                                        exitFailure
                                    Right csv ->
                                        let coneTree = processCSV csv
                                        in outputConeTree f coneTree
                                return ()
                        _ -> putStrLn $ usageInfo header options
                else
                    case files of
                        (f : _) -> do
                                let root         =  case filter (\x -> case x of
                                                                    RootId _ -> True
                                                                    _        -> False) o of
                                                        []           -> UNode (pack "http://www.w3.org/2002/07/owl#Thing")
                                                        RootId r:_ -> UNode (pack r)
                                iconGuesser <- newIconGuesser condIconDir
                                triples <- parseTripes f
                                let
                                    coneTree = processTriples triples lang root
                                    coneTree' = applyIconGuesser iconGuesser coneTree
                                outputConeTree f coneTree'
                        _ -> putStrLn $ usageInfo header options

parseTripes :: FilePath -> IO HashMapS
parseTripes fn =
    case takeExtension fn of
        ".nt"  -> trace "ntriples" $ fmap fromEither (parseFile NTriplesParser fn)
        ".rdf" -> fmap fromEither (parseFile (XmlParser Nothing Nothing) fn)
        ".ttl" -> fmap fromEither (parseFile (TurtleParser Nothing Nothing) fn)
        _      -> do
            putStrLn $ usageInfo header options
            exitFailure

outputConeTree :: FilePath -> ConeTree -> IO ()
outputConeTree f coneTree = do
    let newFilePath = replaceExtension f ".json"
    BS.writeFile newFilePath (encodePretty $ fromConeTree coneTree)

typeUri     = pack "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
kingdom     = pack "http://rdf.geospecies.org/ont/geospecies#KingdomConcept"
inKingdom   = pack "http://rdf.geospecies.org/ont/geospecies#inKingdom"
phylum      = pack "http://rdf.geospecies.org/ont/geospecies#PhylumConcept"
inPhylum    = pack "http://rdf.geospecies.org/ont/geospecies#inPhylum"
bioClass    = pack "http://rdf.geospecies.org/ont/geospecies#ClassConcept"
inClass     = pack "http://rdf.geospecies.org/ont/geospecies#inClass"
order       = pack "http://rdf.geospecies.org/ont/geospecies#OrderConcept"
inOrder     = pack "http://rdf.geospecies.org/ont/geospecies#inOrder"
family      = pack "http://rdf.geospecies.org/ont/geospecies#FamilyConcept"
inFamily    = pack "http://rdf.geospecies.org/ont/geospecies#inFamily"
species     = pack "http://rdf.geospecies.org/ont/geospecies#SpeciesConcept"
label       = pack "http://rdf.geospecies.org/ont/geospecies#hasCanonicalName"
altLabel    = pack "http://purl.org/dc/terms/title"

processTriples :: HashMapS -> String -> Node -> ConeTree
processTriples triples lang root =
    let allKingdom   = map (makeCone triples "Kingdom" . subjectOf) $
                                query triples Nothing (Just (UNode typeUri)) (Just (UNode kingdom))
        allPhylum    = map (makeCone triples "Phylum" . subjectOf) $
                                query triples Nothing (Just (UNode typeUri)) (Just (UNode phylum))
        allClass     = map (makeCone triples "Class" . subjectOf) $
                                query triples Nothing (Just (UNode typeUri)) (Just (UNode bioClass))
        allOrder     = map (makeCone triples "Order" . subjectOf) $
                                query triples Nothing (Just (UNode typeUri)) (Just (UNode order))
        allFamily    = map (makeCone triples "Family" . subjectOf) $
                                query triples Nothing (Just (UNode typeUri)) (Just (UNode family))
        allSpecies   = map (makeCone triples "Species" . subjectOf) $
                                query triples Nothing (Just (UNode typeUri)) (Just (UNode species))
        families     = foldl' (addTo triples inFamily) allFamily allSpecies
        orders       = foldl' (addTo triples inOrder) allOrder families
        classes      = foldl' (addTo triples inClass) allClass orders
        phylus       = foldl' (addTo triples inPhylum) allPhylum classes
        kingdoms     = foldl' (addTo triples inKingdom) allKingdom phylus
    in   trace ("allKingdom size: " ++ show (length allKingdom)) $
         trace ("allPhylum size: " ++ show (length allPhylum)) $
         trace ("allClass size: " ++ show (length allClass)) $
         trace ("allOrder size: " ++ show (length allOrder)) $
         trace ("allFamily size: " ++ show (length allFamily)) $
         trace ("allSpecies size: " ++ show (length allSpecies)) $
         trace ("families size: " ++ show (length families)) $
         trace ("orders size: " ++ show (length orders)) $
         trace ("classes size: " ++ show (length classes)) $
         trace ("phylus size: " ++ show (length phylus)) $
         trace ("kingdoms size: " ++ show (length kingdoms)) $
             RoseLeaf { roseLeaf      = rootEntry
                , roseMeta      = -1
                , roseChildren  = kingdoms
                }
  where rootEntry = ConeEntry
              { ceEntryId     = 0
              , ceLabel       = pack ""
              , ceTargetUri   = Nothing
              , ceComment     = Nothing
              , ceIconName    = Nothing
              , ceStlName     = Nothing
              , ceColor       = Nothing
              , ceIsLeaf      = False
              , ceTextId      = pack "Root"
              }

addTo :: HashMapS ->  Text -> [ConeTree] -> ConeTree -> [ConeTree]
addTo triples selectorUri allParents theTree   =
    let theNode = UNode (ceTextId (roseLeaf theTree))
        selectors = map objectOf $ query triples (Just theNode) (Just (UNode selectorUri)) Nothing
    in foldl' (addTo' theTree) allParents selectors
  where
    addTo' child potentialParents parentNode =
        case partition (\ f -> UNode (ceTextId (roseLeaf f)) == parentNode) potentialParents of
            ([],other)  -> other
            (f:_,other) -> RoseLeaf (roseLeaf f) (-1) (child : roseChildren f) : other


makeCone :: HashMapS -> String -> Node -> ConeTree
makeCone triples typeString subjectNode =
    let entry = makeEntry triples typeString subjectNode
    in
        RoseLeaf
            { roseLeaf      = entry
            , roseMeta      = -1
            , roseChildren  = []
            }

makeEntry :: HashMapS -> String -> Node -> ConeEntry
makeEntry triples typeString subjectNode =
    let allLabels   = query triples (Just subjectNode) (Just (UNode label)) Nothing
        allLabels'  = if null allLabels
                        then query triples (Just subjectNode) (Just (UNode altLabel)) Nothing
                        else allLabels
        label'       = selectLabel (map objectOf allLabels')
        condComment = Nothing
        condIconName = iconNameFor typeString
        textId      = case subjectNode of
                        UNode t -> t
                        n -> pack (show n)
        condTargetUri  = Nothing
        isLeaf = typeString == "Species"
    in ConeEntry
        { ceEntryId     = 0
        , ceLabel       = label'
        , ceTargetUri   = condTargetUri
        , ceComment     = condComment
        , ceIconName    = condIconName
        , ceStlName     = Nothing
        , ceColor       = Nothing
        , ceIsLeaf      = isLeaf
        , ceTextId      = textId
        }

iconNameFor :: String -> Maybe Text
iconNameFor _ = Nothing

selectLabel :: [Node] -> Text
selectLabel [] = pack $ "Unknow"
selectLabel labels =
    case filter filterFunc2 labels of
           LNode (PlainL str) : _ -> str
           _ -> case filter filterFunc3 labels of
                    LNode (PlainLL str _) : _ -> str
                    _ -> pack $ "Unknown"
  where
   -- Find other comment
    filterFunc2 n = case n of
                        (LNode (PlainL str)) -> True
                        _ -> False
    -- Just use the first comment,
    filterFunc3 n = case n of
                        (LNode (PlainLL str _)) -> True
                        _ -> False
