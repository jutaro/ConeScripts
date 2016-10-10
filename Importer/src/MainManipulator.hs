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

import System.Console.GetOpt
import System.Environment
import Data.Version (showVersion)
import Paths_Importer
import qualified Data.ByteString.Lazy.Char8 as BL
import System.FilePath.Posix(takeExtension, replaceExtension, dropExtension)
import System.Exit
import Data.RDF
import Debug.Trace
import Data.Text (pack, unpack, Text)
import qualified Data.ByteString.Lazy as BS(writeFile)
import ConeDemoImport (decodeConeTree)

import ConeServer.Types
import ConeServer.ConeTypes

-- import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
encode = encodePretty

data Flag =    Output FilePath
             | ExplodeWith FilePath

             | Version
             | Help
       deriving (Show,Eq)

options :: [OptDescr Flag]
options =   [
            Option ['o'] ["output"] (ReqArg Output "Path to write output")
              "Pass a filepath of a model"

        ,   Option ['e'] ["explode"] (ReqArg ExplodeWith "Model path")
              "Replace all leafes of the model with empty children with the root children of the given model"

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
header = "Usage: ModelManipulator [OPTION...] fileName"

main = do
    args             <- getArgs
    (o,files)        <- getOpts args
    let condOutput   =  foldr (\x y -> case x of Output d -> Just d; _ -> y) Nothing o
    let explodes     =  foldr (\x y -> case x of ExplodeWith d -> d : y; _ -> y) [] o
    if elem Version o
        then putStrLn $ "ModelManipulator, version " ++ showVersion version
        else if elem Help o
            then putStrLn $ usageInfo header options
            else do
                case files of
                    (fp : _) -> do
                            model <- inputConeTree fp
                            explodeModels <- mapM inputConeTree explodes
                            let newModel = foldl explodeF model explodeModels
                            case condOutput of
                                Just fp -> outputConeTree fp newModel
                                Nothing -> return ()
                    _ -> do
                            putStrLn $ usageInfo header options

explodeF :: ConeTree -> ConeTree -> ConeTree
explodeF originalModel replaceModel =
    let toReplace = roseChildren replaceModel
    in trace ("length toReplace: " ++ show (length toReplace)) $ rebuild toReplace originalModel
  where
    rebuild :: [ConeTree] -> ConeTree -> ConeTree
    rebuild toReplace originalPart =
        case originalPart of
            rl@RoseLeaf
                { roseLeaf      = leaf
                , roseMeta      = m
                , roseChildren  = []
                } -> if ceIsLeaf leaf
                        then rl
                        else rl{roseChildren = toReplace}
            rl@RoseLeaf
                { roseLeaf      = leaf
                , roseMeta      = m
                , roseChildren  = children
                } -> RoseLeaf
                    { roseLeaf      = leaf
                    , roseMeta      = m
                    , roseChildren  = map (rebuild toReplace) children
                    }

inputConeTree :: FilePath -> IO ConeTree
inputConeTree fp = do
    fc <- BL.readFile fp
    case decodeConeTree fc of
        Left errorString -> error "errorString"
        Right model -> trace ("model color!") --  ++ show (ceColor (roseLeaf model)))
                        $ return model

outputConeTree :: FilePath -> ConeTree -> IO ()
outputConeTree f coneTree =
    let coneTree' = applyColorSerialization ColAsWebcolor coneTree
        newFilePath = replaceExtension f ".json"
    in BS.writeFile newFilePath (encode coneTree')
