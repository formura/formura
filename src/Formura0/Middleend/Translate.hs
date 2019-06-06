{-# LANGUAGE OverloadedStrings #-}
module Formura0.Middleend.Translate where

import           Data.Bifunctor
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T

import Formura.GlobalEnvironment
import Formura.NumericalConfig
import Formura.OrthotopeMachine.Graph
import Formura0.Syntax

-- | * 概要
-- AST である Program から OMGraph を生成する
--
-- OMProgram、OMGraph とも新実装を使用する
-- Formura0.Middleend.Translate.Bridge
-- で旧実装に変換して、さらに後ろの処理へ流す

type IdentTable = HM.HashMap IdentName (Int, RExp)

-- |
-- genOMProgram の仕様
-- 1. 型のチェックを行う
-- 2. グローバル変数を特定する
-- 3. Map Ident ValueExpr を構築する (必要??)
-- 4. 各関数についてインライン展開を行い、計算グラフを構築する
--
-- 自分のノードIDと型を返すのがポイント...?
genOMProgram :: Program -> NumericalConfig -> Either String OMProgram
genOMProgram prog cfg = do
  _ <- globalTypeCheck prog
  vs <- findGlobalVariables prog
  t <- makeTable prog
  initGraph <- buildGraph t prog "init"
  stepGraph <- buildGraph t prog "step"
  firstStepGraph <- buildGraph' t prog "first_step"
  filterGraph <- buildGraph' t prog "filter"
  env <- makeGlobalEnvironment prog cfg (calcSleeve stepGraph) (calcSleeve <$> firstStepGraph) (calcSleeve <$> filterGraph)
  return MachineProgram
    { _omGlobalEnvironment = env
    , _omInitGraph = initGraph
    , _omFirstStepGraph = firstStepGraph
    , _omFilterGraph = filterGraph
    , _omStepGraph = stepGraph
    , _omStateSignature = undefined
    }

calcSleeve :: OMGraph -> Int
calcSleeve = undefined

makeTable :: Program -> Either String IdentTable
makeTable = undefined

findGlobalVariables :: Program -> Either String [(IdentName,TExp)]
findGlobalVariables = undefined

makeGlobalEnvironment :: Program -> NumericalConfig -> Int -> Maybe Int -> Maybe Int -> Either String GlobalEnvironment
makeGlobalEnvironment prog cfg sleeve sleeve0 sleeveFilter = do
  cfg' <- bimap show id $ convertConfig sleeve sleeve0 sleeveFilter cfg
  let sp = selectSpecialDecl prog
  dim <- getDim sp
  axes <- getAxes sp dim
  typeName <- getGSTypeName sp
  instanceName <- getGSInstanceName sp
  let bases | dim == 1 = [[1]]
            | dim == 2 = [[1,0],[0,1],[1,1]]
            | dim == 3 = [[1,0,0],[0,1,0],[0,0,1],[1,1,0],[1,0,1],[0,1,1],[1,1,1]]
            | otherwise = error "Not support"
  return GlobalEnvironment
    { _dimension = dim
    , _axesNames = axes
    , _gridStructTypeName = typeName
    , _gridStructInstanceName = instanceName
    , _envNumericalConfig = cfg'
    , _commBases = bases
    }

selectSpecialDecl :: Program -> [SpecialDeclaration]
selectSpecialDecl prog = [ s | (SpcDecl _ s) <- prog ]

getDim :: [SpecialDeclaration] -> Either String Int
getDim sp = case [ d | (Dimension d) <- sp ] of
              [dim] -> Right dim
              []    -> Left "Not found dimension declaration"
              _     -> Left "Multiple dimension declaration"

getAxes :: [SpecialDeclaration] -> Int -> Either String [String]
getAxes sp dim = case [ as | (Axes as) <- sp ] of
                   [as] | length as == dim -> Right (map T.unpack as)
                        | otherwise -> Left "The number of axes does not agree with the dimension"
                   [] -> Left "Not found axes declaration"
                   _ -> Left "Multiple axes declaration"

getGSTypeName :: [SpecialDeclaration] -> Either String String
getGSTypeName sp = case [ tn | (GSTypeName tn) <- sp ] of
                     [n] -> Right (T.unpack n)
                     []  -> Right "Formura_Grid_Struct"
                     _   -> Left "Multiple type name declaration"

getGSInstanceName :: [SpecialDeclaration] -> Either String String
getGSInstanceName sp = case [ n | (GSInstanceName n) <- sp ] of
                         [n] -> Right (T.unpack n)
                         []  -> Right "formura_data"
                         _   -> Left "Multiple instance name declaration"

buildGraph :: IdentTable -> Program -> IdentName -> Either String OMGraph
buildGraph tbl prog name = do
  mgraph <- buildGraph' tbl prog name
  case mgraph of
    Just g  -> Right g
    Nothing -> Left $ "Not found " ++ show (T.unpack name) ++ " function"

buildGraph' :: IdentTable -> Program -> IdentName -> Either String (Maybe OMGraph)
buildGraph' tbl prog name = sequence $ translate tbl <$> lookupGlobalFunction prog name

lookupGlobalFunction :: Program -> IdentName -> Maybe RExp
lookupGlobalFunction prog name = case [ r | (VarDecl _ l r) <- prog, l == IdentL name] of
                                   [rexp] -> Just rexp
                                   _      -> Nothing

-- |
-- 計算グラフ構築の仕様
--
-- 基本的には、構文木を下から走査して遅延評価で上から評価していけばよさそう
-- いまいち、明確になっていないのは
-- - 変数や関数適用をどうやってたどるか -> 記号表的なものを使用するのかな...
-- - タプルをどうやってたどるか; 特に左辺がタプルで右辺が関数適用の場合など
-- あたり?
--
-- 例を考える
-- f = fun(x) 2*x + 1
-- step = fun(q) let a = f(q)
--                   q' = q + a
--                in q'
--
-- これを計算グラフに変換すると
-- 1: Load q
-- 2: Imm 2
-- 3: Imm 1
-- 4: Binop Mul 1 2
-- 5: Binop Add 4 3
-- 6: Binop Add 1 5
-- 7: Store q 7

translate :: IdentTable -> RExp -> Either String OMGraph
translate tbl (LambdaR args body) = undefined
translate _ _                     = Left "Not a function"

-- | グローバル関数の型検査
--   init, step, first_step, filter の引数と返り値の型が一致しているか確認する
globalTypeCheck :: Program -> Either String ()
globalTypeCheck = undefined
