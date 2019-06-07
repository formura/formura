{-# LANGUAGE OverloadedStrings #-}
module Formura0.Middleend.Translate where

import           Data.Bifunctor
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T

import Formura.NumericalConfig
import Formura0.Frontend.Lexer (AlexPosn)
import Formura0.OMGraph
import Formura0.Syntax

-- | * 概要
-- AST である Program から OMGraph を生成する
--
-- OMProgram、OMGraph とも新実装を使用する
-- Formura0.Middleend.Translate.Bridge
-- で旧実装に変換して、さらに後ろの処理へ流す

type IdentTable = HM.HashMap IdentName (AlexPosn, RExp)

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
  t <- makeTable prog
  vs <- findGlobalVariables t
  ig <- buildGraph t prog "init"
  sg <- buildGraph t prog "step"
  fsg <- buildGraph' t prog "first_step"
  flg <- buildGraph' t prog "filter"

  let sp = selectSpecialDecl prog
  dim <- getDim sp
  axes <- getAxes sp dim
  typeName <- getGSTypeName sp
  instanceName <- getGSInstanceName sp
  let bases | dim == 1 = [[1]]
            | dim == 2 = [[1,0],[0,1],[1,1]]
            | dim == 3 = [[1,0,0],[0,1,0],[0,0,1],[1,1,0],[1,0,1],[0,1,1],[1,1,1]]
            | otherwise = error "Not support"

  cfg' <- bimap show id $ convertConfig (calcSleeve sg) (calcSleeve <$> fsg) (calcSleeve <$> flg) cfg
  return OMProgram
    { config = cfg'
    , dimension = dim
    , axesNames = axes
    , gridStructTypeName = typeName
    , gridStructInstanceName = instanceName
    , commBases = bases
    , globalVariables = vs
    , initGraph = ig
    , stepGraph = sg
    , filterGraph = flg
    , firstStepGraph = fsg
    }

calcSleeve :: OMGraph -> Int
calcSleeve = undefined

makeTable :: Program -> Either String IdentTable
makeTable prog = return $ HM.fromList [ (n,(p,exp)) | VarDecl p l r <- prog, let xs = unwrap l r, (n,exp) <- xs]
  where
    unwrap (TupleL xs) r = concat [unwrap x (AppR r (ImmR i)) | (x,i) <- zip xs [0..]]
    unwrap (IdentL n) r  = [(n,r)]
    unwrap (GridL _ l) r = unwrap l r

findGlobalVariables :: IdentTable -> Either String [(IdentName,TExp)]
findGlobalVariables = undefined

selectSpecialDecl :: Program -> [SpecialDeclaration]
selectSpecialDecl prog = [ s | (SpcDecl _ s) <- prog ]

getDim :: [SpecialDeclaration] -> Either String Int
getDim sp = case [ d | (Dimension d) <- sp ] of
              [dim] -> Right dim
              []    -> Left "Not found dimension declaration"
              _     -> Left "Multiple dimension declaration"

getAxes :: [SpecialDeclaration] -> Int -> Either String [IdentName]
getAxes sp dim = case [ as | (Axes as) <- sp ] of
                   [as] | length as == dim -> Right as
                        | otherwise -> Left "The number of axes does not agree with the dimension"
                   [] -> Left "Not found axes declaration"
                   _ -> Left "Multiple axes declaration"

getGSTypeName :: [SpecialDeclaration] -> Either String IdentName
getGSTypeName sp = case [ tn | (GSTypeName tn) <- sp ] of
                     [n] -> Right n
                     []  -> Right "Formura_Grid_Struct"
                     _   -> Left "Multiple type name declaration"

getGSInstanceName :: [SpecialDeclaration] -> Either String IdentName
getGSInstanceName sp = case [ n | (GSInstanceName n) <- sp ] of
                         [n] -> Right n
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
