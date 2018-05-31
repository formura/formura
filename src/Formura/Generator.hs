{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Formura.Generator where

import Control.Lens hiding (op)
import Data.Char (toUpper, toLower)
import Data.Foldable (toList)
import Data.List (isPrefixOf, stripPrefix)
import qualified Data.Map as M
import Data.Scientific
import qualified Data.Text.Lazy.IO as T
import System.Directory
import Text.Heterocephalus (compileTextFile)
import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Printf

import Formura.CommandLineOption
import Formura.GlobalEnvironment
import Formura.NumericalConfig
import Formura.OrthotopeMachine.Graph
import Formura.Syntax
import Formura.Vec

genCode :: WithCommandLineOption => MMProgram -> IO ()
genCode mm = do
  let dim = mm ^. omGlobalEnvironment . dimension
      axes = map (map toUpper) $ toList $ mm ^. omGlobalEnvironment . axesNames
      ic = mm ^. omGlobalEnvironment . envNumericalConfig

  let withAxes :: Vec a -> [(String,a)]
      withAxes = zip axes . toList

  let mpishape = withAxes $ ic ^. icMPIShape
      totalMPI = product $ toList $ ic ^. icMPIShape
      lengthPerNode = withAxes $ fmap (toRealFloat @Double) $ ic ^. icLengthPerNode
      gridPerBlock = withAxes $ ic ^. icGridPerBlock
      blockPerNode = withAxes $ ic ^. icBlockPerNode
      gridPerNode = withAxes $ ic ^. icGridPerNode
      globalSize = concat [printf "[N%s*M%s]" a a | a <- axes] :: String
      ranks = mkRanks dim
      qs = mkIdents $ mm ^. omStateSignature
      nqs = zip [0..] $ map fst qs :: [(Int,String)]
      step = mkStep axes $ mm ^. omStepGraph
      with_simd = False
  let runningScriptPath = "run"
  genRunningScript runningScriptPath totalMPI
  T.writeFile hxxFilePath $ renderMarkup $(compileTextFile "templates/cpu.h")
  T.writeFile cxxFilePath $ renderMarkup $ case dim of
    1 -> let [a1] = axes in $(compileTextFile "templates/cpu-1d.c")
    2 -> let [a1,a2] = axes in $(compileTextFile "templates/cpu-2d.c")
    3 -> let [a1,a2,a3] = axes in $(compileTextFile "templates/cpu-3d.c")
    _ -> error "Not support"
  putStr . unlines $ [ "Generate:"
                     , "  " ++ cxxFilePath
                     , "  " ++ hxxFilePath
                     , "  " ++ runningScriptPath
                     ]

mkRanks :: Int -> [String]
mkRanks d | d == 1 = [ "p1", "m1" ]
          | d == 2 = [ "p1_0", "m1_0", "0_p1", "0_m1", "p1_p1", "m1_m1"]
          | d == 3 = [ "p1_0_0", "m1_0_0"
                     , "0_p1_0", "0_m1_0"
                     , "0_0_p1", "0_0_m1"
                     , "p1_p1_0", "m1_m1_0"
                     , "p1_0_p1", "m1_0_m1"
                     , "0_p1_p1", "0_m1_m1"
                     , "p1_p1_p1", "m1_m1_m1"
                     ]
          | otherwise = error "Not support"

mkIdents :: M.Map IdentName TypeExpr -> [(String,String)]
mkIdents = M.foldrWithKey (\k t acc -> (k, formatType t):acc) []
  where
    formatType :: TypeExpr -> String
    formatType (ElemType "Rational") = "double"
    formatType (ElemType "void") = ""
    formatType (ElemType x) = x
    formatType (GridType _ x) = formatType x
    formatType x = error $ "Unable to translate type to C:" ++ show x

-- Manifestノードの配列サイズを計算する
-- もとの配列サイズ NX+2Ns に比べてどれだけ小さいかを求める
calcSizes :: MMGraph -> M.Map OMNodeID Int
calcSizes = M.foldlWithKey (\acc k (Node mi _ _) -> M.insert k (worker mi acc) acc) M.empty
  where
    worker :: MMInstruction -> M.Map OMNodeID Int -> Int
    worker mi tbl = maximum $ M.foldr go [] mi
      where go (Node (LoadCursorStatic s _) _ _) acc = (maximum $ abs s):acc
            go (Node (LoadCursor s oid) _ _) acc = let s' = maximum $ abs s
                                                       n0 = tbl M.! oid
                                                    in  (n0+s'):acc
            go _ acc = acc

mkStep :: [String] -> MMGraph -> String
mkStep axes mmg = withTmp $ unlines [ genMMInst omid $ if isVoid omt then mm else insertMNStore omid mm | (omid, (Node mm omt _)) <- M.toAscList mmg ]
  where
    sizeTable = calcSizes mmg
    -- 中間変数を生成するかどうかを判定する
    -- 型が void なら最後に Store されているはずなので、中間変数を生成しない
    -- そうでないなら、最後に型に合う変数に結果を書き込む必要がある (最後に命令として Store がない)
    isVoid (ElemType "void") = True
    isVoid _ = False

    insertMNStore :: OMNodeID -> MMInstruction -> MMInstruction
    insertMNStore oid mm = M.insert (MMNodeID mmid) node mm
      where mmid = M.size mm
            node = Node (Store (formatTmp oid) (MMNodeID $ mmid-1)) (ElemType "void") []

    withTmp body = unlines [ genTmpDecl omt omid ++ ";" | (omid, (Node _ omt _)) <- M.toAscList mmg, not (isVoid omt) ] ++ body

    genTmpDecl (ElemType "Rational") oid = "double " ++ formatTmp oid
    genTmpDecl (ElemType x) oid = x ++ " " ++ formatTmp oid
    genTmpDecl (GridType _ x) oid = let s = sizeTable M.! oid
                                    in (genTmpDecl x oid) ++ formatSize s
    genTmpDecl x _ = error $ "Unable to translate type to C:" ++ show x

    genMMInst :: OMNodeID -> MMInstruction -> String
    genMMInst omid mm = inLoop s $ unlines [ genMicroInst mmid mi mt s ++ ";" | (mmid, (Node mi mt _)) <- M.toAscList mm ]
      where s = sizeTable M.! omid

    inLoop s body = foldr (\a acc -> let a' = map toLower a in printf "for(int i%s = 0; i%s < N%s + 2*(Ns - %d); i%s++) {\n%s}\n" a' a' a s a' acc) body axes

    formatType :: MicroNodeType -> String
    formatType (ElemType "Rational") = "double "
    formatType (ElemType "void") = ""
    formatType (ElemType x) = x ++ " "
    formatType _ = error "Invalid type"

    formatIndex :: Vec Int -> String
    formatIndex di = concat [printf "[i%s%+d]" (map toLower a) i | (a,i) <- zip axes $ toList di]

    formatNode :: MMNodeID -> String
    formatNode i = "a" ++ show i

    formatTmp :: OMNodeID -> String
    formatTmp i = "MN" ++ show i

    formatSize :: Int -> String
    formatSize s = concat [printf "[N%s+2*(Ns-%d)]" a s | a <- axes]

    genMicroInst _ (Store name x) _ _ | "MN" `isPrefixOf` name = name ++ formatIndex (Vec [0,0,0]) ++ " = " ++ formatNode x
                                      | otherwise = "rslt->" ++ name ++ formatIndex (Vec [0,0,0]) ++ " = " ++ formatNode x
    genMicroInst mmid mi mt s =
      let lhs = formatType mt ++ formatNode mmid
          rhs = case mi of
                  LoadCursorStatic d name -> "buff->" ++ name ++ formatIndex (d + pure s)
                  LoadCursor d nid -> let s' = sizeTable M.! nid in formatTmp nid ++ formatIndex (d + pure (s-s'))
                  Imm r -> show (realToFrac r :: Double)
                  Uniop op a | "external-call/" `isPrefixOf` op -> let Just f = stripPrefix "extern-call/" op in f ++ "(" ++ formatNode a ++ ")"
                             | otherwise -> op ++ formatNode a
                  Binop op a b | op == "**" -> "pow(" ++ formatNode a ++ "," ++ formatNode b ++ ")"
                               | otherwise -> formatNode a ++ op ++ formatNode b
                  Triop "ite" a b c -> formatNode a ++ "?" ++ formatNode b ++ ":" ++ formatNode c
                  -- LoadIndex をサポートするにはグローバルな配列に対するオフセットが必要
                  -- つまり、Formura_Step のAPIを変更する必要がある
                  -- LoadIndex i -> "i" ++ (map toLower $ axes !! i)
                  -- Naryop は廃止かもなので、実装を待つ
                  -- Naryop op xs -> undefined
                  x -> error $ "Unimplemented for keyword: " ++ show x
      in unwords [lhs, "=", rhs]


genRunningScript :: String -> Int -> IO ()
genRunningScript fn n = do
  let script = unlines ["#!/bin/bash"
                       ,"prog=${1:?Need an executable file path}"
                       ,"opt=${2}"
                       ,"mpirun ${opt} -n " ++ show n ++ " ${prog}"
                       ]
  writeFile fn script
  p <- getPermissions fn
  setPermissions fn $ p {executable = True}
