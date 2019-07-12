{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
module Main where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import qualified Data.Map as M
import Data.Time
import           System.IO
import           System.Exit
import qualified Text.PrettyPrint.ANSI.Leijen as Ppr
import qualified Text.Trifecta as P

import qualified Formura.Annotation as A
import           Formura.Annotation.Representation
import           Formura.CommandLineOption
import           Formura.Desugar
import           Formura.NumericalConfig
import           Formura.IR
import           Formura.Generator (genCode)
import           Formura.OrthotopeMachine.Graph
import           Formura.OrthotopeMachine.Manifestation (genMMProgram)
import           Formura.OrthotopeMachine.Translate (genOMProgram)
import qualified Formura.Parser as P
import           Formura.Syntax

bench :: String -> IO ()
bench msg = do
  t <- getCurrentTime
  putStrLn $ msg ++ " (" ++ formatTime defaultTimeLocale "%T" t ++ ")"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  bench "start"
  opts <- getCommandLineOption
  let ?commandLineOption = opts

  mapM_ process (opts ^. inputFilenames)


process :: WithCommandLineOption => FilePath -> IO ()
process fn = do
  bench "process"
  nc <- readConfig ncFilePath
          `catches` [Handler (\(_ :: IOException) -> die $ "Error: Unable to read " ++ ncFilePath)
                    ,Handler (\(e :: ConfigException) -> die $ "Error: " ++ displayException e)
                    ]
  mprog <- {-# SCC "parse:" #-} P.parseFromFileEx (P.runP $ P.program nc <* P.eof) fn
  case mprog of
    P.Failure err -> do
      Ppr.displayIO stderr $ Ppr.renderPretty 0.8 80 $ P._errDoc err <> Ppr.linebreak
      exitFailure
    P.Success prog -> codegen prog


codegen :: WithCommandLineOption => Program -> IO ()
codegen sugarcoated_prog = do
  bench "codegen"
  prog <- desugar sugarcoated_prog
  when (?commandLineOption ^. verbose) $ do
    putStrLn "## AST"
    print sugarcoated_prog
    putStrLn ""
    putStrLn "## Desugared AST"
    print prog
    putStrLn ""

  bench "genOMProgram"
  omProg <- {-# SCC "genOMProgram:" #-} genOMProgram prog

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

  bench "genMMProgram"
  mmProg <- {-# SCC "genMMProgram:" #-} genMMProgram omProg
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

  bench "genIRProgram"
  let irProg = {-# SCC "genIRProgram:" #-} genIRProgram mmProg
  when (?commandLineOption ^. verbose) $ do
    putStrLn "## Debug print: simulation state"
    print (irProg ^. irStateSignature)
    putStrLn ""

    putStrLn "## Debug print: manifested init graph"
    print (irProg ^. irInitGraph)
    putStrLn ""

    putStrLn "## Debug print: manifested step graph"
    print (irProg ^. irStepGraph)
    putStrLn ""

  bench "genCode"
  putStrLn "Generating code..."
  genCode irProg
  bench "Done"

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
