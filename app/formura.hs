{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
module Main where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import qualified Data.Map as M
import           System.IO
import           System.Exit
import qualified Text.PrettyPrint.ANSI.Leijen as Ppr
import qualified Text.Trifecta as P

import qualified Formura.Annotation as A
import           Formura.Annotation.Representation
import           Formura.CommandLineOption
import           Formura.Desugar
import           Formura.NumericalConfig
import           Formura.Generator (genCode)
import           Formura.OrthotopeMachine.Graph
import           Formura.OrthotopeMachine.Manifestation (genMMProgram)
import           Formura.OrthotopeMachine.Translate (genOMProgram)
import qualified Formura.Parser as P
import           Formura.Syntax

main :: IO ()
main = do
  opts <- getCommandLineOption
  let ?commandLineOption = opts

  mapM_ process (opts ^. inputFilenames)


process :: WithCommandLineOption => FilePath -> IO ()
process fn = do
  nc <- readConfig ncFilePath
          `catches` [Handler (\(_ :: IOException) -> die $ "Error: Unable to read " ++ ncFilePath)
                    ,Handler (\(e :: ConfigException) -> die $ "Error: " ++ displayException e)
                    ]
  mprog <- P.parseFromFileEx (P.runP $ P.program nc <* P.eof) fn
  case mprog of
    P.Failure err -> do
      Ppr.displayIO stderr $ Ppr.renderPretty 0.8 80 $ P._errDoc err <> Ppr.linebreak
      exitFailure
    P.Success prog -> codegen prog


codegen :: WithCommandLineOption => Program -> IO ()
codegen sugarcoated_prog = do
  prog <- desugar sugarcoated_prog
  when (?commandLineOption ^. verbose) $ do
    putStrLn "## AST"
    print sugarcoated_prog
    putStrLn ""
    putStrLn "## Desugared AST"
    print prog
    putStrLn ""

  omProg <- genOMProgram prog

  when (?commandLineOption ^. verbose) $ do
    putStrLn "## Debug print: simulation state"
    print (omProg ^. omStateSignature)
    putStrLn ""

    putStrLn "## Debug print: init graph"
    mapM_ pprNode $ M.toList (omProg ^. omInitGraph)
    putStrLn ""

    putStrLn "## Debug print: step graph"
    mapM_ pprNode $ M.toList (omProg ^. omStepGraph)
    putStrLn ""

    putStrLn "## Debug print: global environment of the simulation"
    print (omProg ^. omGlobalEnvironment)
    putStrLn ""

  mmProg <- genMMProgram omProg
  when (?commandLineOption ^. verbose) $ do
    putStrLn "## Debug print: simulation state"
    print (mmProg ^. omStateSignature)
    putStrLn ""

    putStrLn "## Debug print: manifested init graph"
    mapM_ pprMMNode $ M.toList (mmProg ^. omInitGraph)
    putStrLn ""

    putStrLn "## Debug print: manifested step graph"
    mapM_ pprMMNode $ M.toList (mmProg ^. omStepGraph)
    putStrLn ""

  putStrLn "Generating code..."
  genCode mmProg

pprNode :: (OMNodeID, OMNode) -> IO ()
pprNode (i,n) = do
  let r = case A.toMaybe (n ^. A.annotation) of
        Just Manifest -> "M"
        _             -> " "
      varName = case A.toMaybe (n ^. A.annotation) of
        Just (SourceName n1) -> n1
        _                   -> ""
  putStrLn $ unwords [r , take 8 $ varName ++ repeat ' ', show (i,n)]


pprMMNode :: (OMNodeID, MMNode) -> IO ()
pprMMNode (i,n) = do
  let
      varName = case A.toMaybe (n ^. A.annotation) of
        Just (SourceName n1) -> n1
        _                   -> ""
      -- Just (Boundary bdy) = A.toMaybe $ n^.A.annotation
  -- print bdy
  -- putStrLn $ unwords [take 8 $ varName ++ repeat ' ', show (i,n),show bdy]
  putStrLn $ unwords [take 8 $ varName ++ repeat ' ', show (i,n)]
