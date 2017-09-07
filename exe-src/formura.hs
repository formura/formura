{-# LANGUAGE ImplicitParams #-}
module Main where


import           Control.Lens
import           Control.Monad
import qualified Data.Map as M
import           Data.Monoid
import           System.IO
import qualified Text.PrettyPrint.ANSI.Leijen as Ppr
import qualified Text.Trifecta as P

import qualified Formura.Annotation as A
import           Formura.Annotation.Boundary
import           Formura.Annotation.Representation
import           Formura.CommandLineOption
import           Formura.OrthotopeMachine.Graph
import           Formura.OrthotopeMachine.Translate (genOMProgram)
import           Formura.OrthotopeMachine.Manifestation (genMMProgram)
import qualified Formura.Parser as P
import           Formura.Desugar
import           Formura.Syntax
import           Formura.MPICxx.Language (TargetLanguage(..), targetLanguage)
import qualified Formura.MPICxx.Translate as C
import qualified Formura.MPIFortran.Translate as F

main :: IO ()
main = do
  opts <- getCommandLineOption
  let ?commandLineOption = opts

  mapM_ process (opts ^. inputFilenames)


process :: WithCommandLineOption => FilePath -> IO ()
process fn = do
  mprog <- P.parseFromFileEx (P.runP $ P.program <* P.eof) fn
  case mprog of
      P.Failure err -> Ppr.displayIO stdout $ Ppr.renderPretty 0.8 80 $ P._errDoc err <> Ppr.linebreak
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
    putStrLn "## Debug print: global environment of the simulation"
    print (omProg ^. omGlobalEnvironment)
    putStrLn ""

    putStrLn "## Debug print: simulation state"
    print (omProg ^. omStateSignature)
    putStrLn ""

    putStrLn "## Debug print: init graph"
    mapM_ pprNode $ M.toList (omProg ^. omInitGraph)
    putStrLn ""

    putStrLn "## Debug print: step graph"
    mapM_ pprNode $ M.toList (omProg ^. omStepGraph)
    putStrLn ""

  mmProg <- genMMProgram omProg
  when (?commandLineOption ^. verbose) $ do
    putStrLn "## Debug print: manifested init graph"
    mapM_ pprMMNode $ M.toList (mmProg ^. omInitGraph)
    putStrLn ""

    putStrLn "## Debug print: manifested step graph"
    mapM_ pprMMNode $ M.toList (mmProg ^. omStepGraph)
    putStrLn ""

  putStrLn $ "Target language is:" ++ show targetLanguage
  case targetLanguage of
    MPICxx -> C.genCxxFiles prog mmProg
    MPIFortran -> F.genFortranFiles prog mmProg

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
