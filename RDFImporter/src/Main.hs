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
-- | Script for importing rdf files in N-Triples, Turtle and RDF/XML
-- and exporting as ConeServer model file
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import IconGuesser

import System.Console.GetOpt
import System.Environment
import Data.Version (showVersion)
import Paths_RDFImporter
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

-- import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
encode = encodePretty


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
    BS.writeFile newFilePath (encode coneTree)

typeUri = pack "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
classUri = pack "http://www.w3.org/2002/07/owl#Class"
labelUri = pack "http://www.w3.org/2000/01/rdf-schema#label"
commentUri = pack "http://www.w3.org/2000/01/rdf-schema#comment"
subclassUri = pack "http://www.w3.org/2000/01/rdf-schema#subClassOf"
datatypePropertyUri = pack "http://www.w3.org/2002/07/owl#DatatypeProperty"
objectPropertyUri = pack "http://www.w3.org/2002/07/owl#ObjectProperty"
domainProperty = pack "http://www.w3.org/2000/01/rdf-schema#domain"

processTriples :: HashMapS -> String -> Node -> ConeTree
processTriples triples lang root =
    let allClasses    = map subjectOf $
                            query triples Nothing (Just (UNode typeUri)) (Just (UNode classUri))
        allClassNodes = if root `elem` allClasses
                            then allClasses
                            else root : allClasses
        classEntries  = map (makeClassEntry triples lang) allClassNodes
        classesWithoutSuperclasses = filter (hasNoSuperclass triples) allClassNodes
        datatypeProperties = map subjectOf $
                                query triples Nothing (Just (UNode typeUri)) (Just (UNode datatypePropertyUri))
        objectProperties = map subjectOf $
                                query triples Nothing (Just (UNode typeUri)) (Just (UNode objectPropertyUri))
        properties = datatypeProperties ++ objectProperties
        propertyEntries = map (makePropertyEntry triples lang) properties
        coneTree      = buildTreeFrom root triples allClassNodes classEntries propertyEntries
    in trace ("Found " ++ show (length allClasses) ++ " classes.")
--       $ trace ("Made entries " ++ show (length classEntries)) $
--       $ trace ("Classes without super " ++ show classesWithoutSuperclasses) $
--       $ trace ("Properties " ++ show (length properties)) $
            coneTree


buildTreeFrom :: Node -> HashMapS -> [Node] -> [ConeEntry] -> [ConeEntry] -> ConeTree
buildTreeFrom root triples allClasses classEntries propertyEntries = constructNodeFor root
  where
    constructNodeFor node =
        let entry = entryFor node
            properties = propertiesFor node
            children = findSubclassesFor node
            childLeafs = map constructNodeFor children
        in RoseLeaf
                { roseLeaf      = entry
                , roseMeta      = 0
                , roseChildren  = childLeafs ++ map makeRoseLeaf properties
                }

    entryFor node = case filter (\ entry -> case node of
                                                UNode t -> t == ceTextId entry
                                                _ -> False) classEntries of
                        [entry] -> entry
                        l -> error ("EntryFor node: " ++ show node ++ "not consistent with: " ++ show l)

    propertiesFor node =
        let props = map subjectOf $ query triples  Nothing (Just (UNode domainProperty)) (Just node)
        in mapMaybe propFor props

    propFor propNode =  case filter (\ entry -> case propNode of
                                                    UNode t -> t == ceTextId entry
                                                    _ -> False) propertyEntries of
                        [entry] -> Just entry
                        l -> Nothing

    findSubclassesFor :: Node -> [Node]
    findSubclassesFor node = map subjectOf $ query triples  Nothing (Just (UNode subclassUri)) (Just node)

hasNoSuperclass :: HashMapS -> Node -> Bool
hasNoSuperclass triples node = null $ query triples (Just node) (Just (UNode subclassUri)) Nothing


makeClassEntry :: HashMapS -> String -> Node -> ConeEntry
makeClassEntry triples lang classNode =
    let allLabels   = query triples (Just classNode) (Just (UNode labelUri)) Nothing
        label       = selectLabel (map objectOf allLabels) lang classNode
        allComments = query triples (Just classNode) (Just (UNode commentUri)) Nothing
        allCommentNodes = map objectOf allComments
        condComment = case getForLanguage "en" allCommentNodes of
                        Just c -> Just c
                        Nothing ->case filter (\ n -> case n of
                                                LNode (PlainL str) -> True
                                                _ -> False)
                                            allCommentNodes of
                                        (LNode (PlainL str):_) -> Just str
                                        _ -> Nothing
        textId      = case classNode of
                        UNode t -> t
                        n -> pack (show n)
        condTargetUri  = case classNode of
                        UNode t -> Just t
                        n -> Nothing
    in ConeEntry
        { ceEntryId     = 0
        , ceLabel       = label
        , ceTargetUri   = condTargetUri
        , ceComment     = condComment
        , ceIconName    = Nothing
        , ceStlName     = Nothing
        , ceColor       = Nothing
        , ceIsLeaf      = False
        , ceTextId      = textId
        }

makePropertyEntry :: HashMapS -> String -> Node -> ConeEntry
makePropertyEntry triples lang propertyNode =
    let allLabels   = query triples (Just propertyNode) (Just (UNode labelUri)) Nothing
        label       = selectLabel (map objectOf allLabels) lang propertyNode
        allComments = query triples (Just propertyNode) (Just (UNode commentUri)) Nothing
        allCommentNodes = map objectOf allComments
        condComment = case getForLanguage "en" allCommentNodes of
                        Just c -> Just c
                        Nothing ->case filter (\ n -> case n of
                                                LNode (PlainL str) -> True
                                                _ -> False)
                                            allCommentNodes of
                                        (LNode (PlainL str):_) -> Just str
                                        _ -> Nothing
        textId      = case propertyNode of
                        UNode t -> t
                        n -> pack (show n)
        condTargetUri  = case propertyNode of
                        UNode t -> Just t
                        n -> Nothing
    in ConeEntry
        { ceEntryId     = 0
        , ceLabel       = label
        , ceTargetUri   = condTargetUri
        , ceComment     = condComment
        , ceIconName    = Nothing
        , ceStlName     = Nothing
        , ceColor       = Nothing
        , ceIsLeaf      = True
        , ceTextId      = textId
        }

makeRoseLeaf :: ConeEntry -> ConeTree
makeRoseLeaf entry =
    RoseLeaf
        { roseLeaf      = entry
        , roseMeta      = 0
        , roseChildren  = []
        }


getForLanguage :: String -> [Node] -> Maybe Text
getForLanguage lang nodes =
    case filter filterFunc nodes of
        (LNode (PlainLL str _):_) -> Just str
        _ -> Nothing
  where
    filterFunc n = case n of
                      (LNode (PlainLL _ l)) | l == pack lang -> True
                      _ -> False

selectLabel :: [Node] -> String -> Node -> Text
selectLabel [] lang classNode = pack $ show classNode
selectLabel labels lang classNode =
    fromMaybe
      (case filter filterFunc2 labels of
           LNode (PlainL str) : _ -> str
           _ -> case filter filterFunc3 labels of
                    LNode (PlainLL str _) : _ -> str
                    _ -> pack $ show classNode)
      (getForLanguage lang labels)
  where
   -- Find other comment
    filterFunc2 n = case n of
                        (LNode (PlainL str)) -> True
                        _ -> False
    -- Just use the first comment,
    filterFunc3 n = case n of
                        (LNode (PlainLL str _)) -> True
                        _ -> False
