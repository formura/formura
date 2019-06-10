{-# LANGUAGE OverloadedStrings #-}
module Formura0.Middleend.Translate where

import           Data.Bifunctor
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T

import Formura.NumericalConfig
import Formura0.Frontend.Lexer (AlexPosn)
import Formura0.OMGraph
import Formura0.Syntax
import Formura0.Utils

-- | * 概要
-- AST である Program から OMGraph を生成する
--
-- OMProgram、OMGraph とも新実装を使用する
-- Formura0.Middleend.Translate.Bridge
-- で旧実装に変換して、さらに後ろの処理へ流す

type IdentTable = HM.HashMap IdentName (AlexPosn, RExp)
type TypeTable = HM.HashMap IdentName TExp

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
  t <- makeIdentTable prog
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

makeIdentTable :: Program -> Either String IdentTable
makeIdentTable prog = return $ HM.fromList [ (n,(p,e)) | VarDecl p l r <- prog, (n,e) <- unwrap l r]
  where
    unwrap (TupleL xs) r = concat [unwrap x (AppR r (ImmR i)) | (x,i) <- zip xs [0..]]
    unwrap (IdentL n) r  = [(n,r)]
    unwrap (GridL _ l) r = unwrap l r

makeTypeTable :: Program -> Either String TypeTable
makeTypeTable prog = return $ HM.fromList [ (n,t') | (TypeDecl _ (ModifiedType _ t) l) <- prog, (n,t') <- unwrap l t]
  where
    unwrap (IdentL n) t            = [(n,t)]
    unwrap (GridL _ l) t           = unwrap l t
    unwrap (TupleL xs) (TupleT ts) = concat [unwrap x t | (x,t) <- zip xs ts]
    unwrap (TupleL xs) SomeType    = concat [unwrap x SomeType | x <- xs]
    unwrap _ _                     = error "error" -- FIX ME

typeOf :: IdentName -> TypeTable -> Either String TExp
typeOf n tbl = case HM.lookup n tbl of
                 Nothing -> Left $ "Not found " ++ show (T.unpack n)
                 Just t  -> Right t

findGlobalVariables :: IdentTable -> Either String [(IdentName,TExp)]
findGlobalVariables tbl =
  case HM.lookup "init" tbl of
    Nothing              -> Left "Error: Not found init function"
    Just (p,LambdaR _ r) ->
      case r of
        (LetR b (IdentR x)) -> do
          tt <- makeTypeTable b
          t <- typeOf x tt
          return [(x, t)]
        (LetR b (TupleR xs)) -> do
          tt <- makeTypeTable b
          xs' <- unwrap xs
          ts <- sequence [typeOf x tt | x <- xs']
          return $ zip xs' ts
        _ -> Left $ "Error: " ++ formatPos p ++ " init must return a tuple of grid"
    Just (p,_)           -> Left $ "Error:" ++ formatPos p ++ " init must be a function"
  where
    unwrap xs = case [n | (IdentR n) <- xs] of
                  ys | length ys == length xs -> Right ys
                     | otherwise -> Left "init must return a tuple of grid"

namesOfLExp :: LExp -> [IdentName]
namesOfLExp (IdentL n)  = [n]
namesOfLExp (TupleL xs) = concatMap namesOfLExp xs
namesOfLExp (GridL _ x) = namesOfLExp x

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
