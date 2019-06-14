{-# LANGUAGE OverloadedStrings #-}
module Formura0.Middleend.Translate where

import           Data.Bifunctor (bimap)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Tree

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
type TypeTable = HM.HashMap IdentName ([TypeModifier],TExp)

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
  iTbl <- makeIdentTable prog
  tTbl <- makeTypeTable prog
  vs <- findGlobalVariables iTbl
  ig <- buildGraph iTbl tTbl vs "init"
  sg <- buildGraph iTbl tTbl vs "step"
  fsg <- buildGraph' iTbl tTbl vs "first_step"
  flg <- buildGraph' iTbl tTbl vs "filter"

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
makeTypeTable prog = return $ HM.fromList [ (n,(ms,t')) | (TypeDecl _ (ModifiedType ms t) l) <- prog, (n,t') <- unwrap l t]
  where
    unwrap (IdentL n) t            = [(n,t)]
    unwrap (GridL _ l) t           = unwrap l t
    unwrap (TupleL xs) (TupleT ts) = concat [unwrap x t | (x,t) <- zip xs ts]
    unwrap (TupleL xs) SomeType    = concat [unwrap x SomeType | x <- xs]
    unwrap _ _                     = error "error" -- FIX ME

typeOf :: IdentName -> TypeTable -> Either String TExp
typeOf n tbl = case HM.lookup n tbl of
                 Nothing -> Left $ "Not found " ++ show (T.unpack n)
                 Just t  -> Right (snd t)

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

buildGraph :: IdentTable -> TypeTable -> GlobalVariables -> IdentName -> Either String OMGraph
buildGraph iTbl tTbl vs name = do
  mgraph <- buildGraph' iTbl tTbl vs name
  case mgraph of
    Just g  -> Right g
    Nothing -> Left $ "Not found " ++ show (T.unpack name) ++ " function"

buildGraph' :: IdentTable -> TypeTable -> GlobalVariables -> IdentName -> Either String (Maybe OMGraph)
buildGraph' iTbl tTbl vs name = sequence $ translate iTbl tTbl vs <$> lookupGlobalFunction iTbl name

lookupGlobalFunction :: IdentTable -> IdentName -> Maybe RExp
lookupGlobalFunction tbl name = snd <$> HM.lookup name tbl

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
--
--
-- 検討点
-- - ローカルスコープの実現
-- - タプルの処理
-- - グローバル変数の load と store
--

translate :: IdentTable -> TypeTable -> GlobalVariables -> RExp -> Either String OMGraph
translate iTbl tTbl vs (LambdaR args (LetR b xs)) = do
  typecheck vs args xs
  iTbl' <- makeIdentTable b
  tTbl' <- makeTypeTable b
  (g,ids,_) <- trans (iTbl <> iTbl') (tTbl <> tTbl') vs xs
  storeResult g ids vs
translate _ _ _ _                      = Left "Not a function"

-- |
-- typecheck は、グローバル関数の引数と返り値の数がグローバル変数と一致するか調べる
typecheck :: GlobalVariables -> LExp -> RExp -> Either String ()
typecheck vs (TupleL ls) (TupleR rs) = if n == length ls && n == length rs then return () else Left "mismatch the length of tuples"
  where n = length vs
typecheck _ _ _ = Left "must be a tuple to tuple function"

trans :: IdentTable -> TypeTable -> GlobalVariables -> RExp -> Either String (OMGraph, Tree OMID, TExp)
trans iTbl tTbl vs r = undefined

storeResult :: OMGraph -> Tree OMID -> GlobalVariables -> Either String OMGraph
storeResult g ids vs = return $ foldl (\g0 (i,v) -> store i v g0) g $ zip (flatten ids) (map fst vs)
  where
    store i v g0 = let i' = M.size g0
                       node = OMNode { inst = Store v i
                                     , theType = IdentT "void"
                                     , annot = []
                                     }
                    in M.insert i' node g0

-- |
-- trans の仕様
-- IdentTable を参照しながら、 RExp を評価して OMGraph に挿入していく
--
-- Annotation の扱いに注意
-- - 外部関数
-- - Manifest 変数
--
-- trans :: IdentTable -> RExp -> Either String OMGraph
-- trans tbl r = (\(a,_,_) -> a) <$> go M.empty r
--   where
    -- FIXME:
    -- - GlobalVariables の扱いを考えなおす (initGraph での構築ではダメでは...)
    -- - LetR や AppR を考えると IdentTable も go がもちまわる必要がある
    -- - TupleR を考えると [OMID] でないとダメでは...
    -- go :: OMGraph -> RExp -> Either String (OMGraph, [OMID], TExp)
    -- go g0 (ImmR x) = let i = M.size g0
    --                      t = IdentT "double"
    --                      node = OMNode { inst = Imm x
    --                                    , theType = t
    --                                    , annot = []
    --                                    }
    --                      g = M.insert i node g0
    --                   in return (g,[i],t)
    -- go g0 (IdentR n) = case HM.lookup n tbl of
    --                      Nothing -> Left $ "Not found the identifier " ++ show (T.unpack n)
    --                      Just (_,r) -> go g0 r
    -- go g0 (TupleR rs) = undefined
    -- go g0 (UniopR op r) = do
    --   (g,i,t) <- go g0 r
    --   let i' = M.size g
    --       node = OMNode { inst = Uniop op i
    --                     , theType = t
    --                     , annot = []
    --                     }
    --       g' = M.insert i' node g
    --   return (g',i',t)
    -- go g0 (BinopR op r1 r2) = do
    --   (g1,i1,t1) <- go g0 r1
    --   (g2,i2,t2) <- go g1 r2
    --   matchType t1 t2
    --   let i = M.size g2
    --       t = inferType op t1
    --       node = OMNode { inst = Binop op i1 i2
    --                     , theType = t
    --                     , annot = []
    --                     }
    --       g' = M.insert i node g2
    --   return (g',i,t)
    -- go g0 (IfR cond r1 r2) = do
    --   (g1,i1,t1) <- go g0 cond
    --   (g2,i2,t2) <- go g1 r1
    --   (g3,i3,t3) <- go g2 r2
    --   matchType t1 (IdentT "bool")
    --   matchType t2 t3
    --   let i = M.size g3
    --       node = OMNode { inst = If i1 i2 i3
    --                     , theType = t2
    --                     , annot = []
    --                     }
    --       g' = M.insert i node g3
    --   return (g',i,t2)

matchType :: TExp -> TExp -> Either String ()
matchType t1 t2 | t1 == t2 = return ()
                | otherwise = Left $ "Not match types: " ++ show t1 ++ " /= " ++ show t2

inferType :: Op2 -> TExp -> TExp
inferType op t = if isArith op then t else IdentT "bool"
  where
    isArith o = o `elem` [Add,Sub,Mul,Div,Pow]

-- | グローバル関数の型検査
--   init, step, first_step, filter の引数と返り値の型が一致しているか確認する
globalTypeCheck :: Program -> Either String ()
globalTypeCheck = undefined
