{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Formura0.Middleend.Translate where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Bifunctor (bimap)
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import           Data.Ratio
import qualified Data.Text as T

import Formura.NumericalConfig
import Formura0.Annotation
import Formura0.Frontend.Lexer (AlexPosn)
import Formura0.OMGraph
import Formura0.Syntax
import Formura0.Utils
import Formura0.Vec

-- | * 概要
-- AST である Program から OMGraph を生成する
--
-- OMProgram、OMGraph とも新実装を使用する
-- Formura0.Middleend.Translate.Bridge
-- で旧実装に変換して、さらに後ろの処理へ流す

data Value = ValueR RExp
           | ValueI !Int
           | ValueN (Tree OMID) TExp
  deriving (Eq,Show)

-- [IdentName] はグリッドのインデックス
-- スコープを正しく実装するため、各右辺式を評価するときに IdentTable へ組み込む
type IdentTable = HM.HashMap IdentName (AlexPosn, [IdentName], Value)
type TypeTable = HM.HashMap IdentName ([TypeModifier],TExp)

-- |
-- new にも old にも同じキーが存在する場合、 old のほうが消え、 new のほうが残る
(|+>) :: (Eq k, Hashable k) => HM.HashMap k v -> HM.HashMap k v -> HM.HashMap k v
new |+> old = new <> old

data Tree a = Leaf !a
            | Node [Tree a]
  deriving (Eq,Show,Functor)

instance Foldable Tree where
  foldr f acc (Leaf x)      = f x acc
  foldr _ acc (Node [])     = acc
  foldr f acc (Node (x:xs)) = foldr f (foldr f acc x) (Node xs)

flatten :: Tree a -> [a]
flatten (Leaf a)  = [a]
flatten (Node ts) = concatMap flatten ts

data Env = Env
  { identTable :: !IdentTable
  , typeTable  :: !TypeTable
  , gVariables :: !GlobalVariables
  , sourcePos  :: !AlexPosn
  } deriving (Show)

type TransError = String

newtype TransM a = TransM { runTrans :: ReaderT Env (StateT OMGraph (Either TransError)) a}
  deriving ( Functor, Applicative, Monad
           , MonadReader Env
           , MonadState OMGraph
           , MonadError TransError
           )

run :: IdentTable -> TypeTable -> GlobalVariables -> AlexPosn -> TransM a -> Either TransError OMGraph
run iTbl tTbl vs p act = execStateT (runReaderT (runTrans act) env) M.empty
  where
    env = Env iTbl tTbl vs p

reportError :: TransError -> TransM a
reportError msg = do
  e <- ask
  g <- get
  throwError $
    unlines [ "Error: " ++ formatPos (sourcePos e) ++ " " ++ msg
            , "  reported in translate"
            , "  env:"
            , "    " ++ show e
            , ""
            , "  graph:"
            , "    " ++ show g
            ]

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
makeIdentTable prog = return $ HM.fromList [ (n,(p,idx,ValueR e)) | VarDecl p l r <- prog, (n,!idx,e) <- unwrap l r]
  where
    unwrap (TupleL xs) r = concat [unwrap x (AppR r (ImmR i)) | (x,i) <- zip xs [0..]]
    unwrap (IdentL n) r  = [(n,[],r)]
    unwrap (GridL (Vec (ZipList idx)) l) r = [(n,idx++idx',r') | (n,idx',r') <- unwrap l r]

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
    Just (p,_,ValueR (LambdaR _ r)) ->
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
    Just (p,_,_)           -> Left $ "Error:" ++ formatPos p ++ " init must be a function"
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
buildGraph' iTbl tTbl vs name = sequence $ translate iTbl tTbl vs . (\(p,_,ValueR r) -> (p,r)) <$> HM.lookup name iTbl

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

translate :: IdentTable -> TypeTable -> GlobalVariables -> (AlexPosn,RExp) -> Either String OMGraph
translate iTbl tTbl vs (p,LambdaR args (LetR b xs)) = do
  typecheck vs args xs
  iTbl' <- makeIdentTable b
  tTbl' <- makeTypeTable b
  -- (g,ids,_) <- trans (iTbl <> iTbl') (tTbl <> tTbl') vs M.empty (TupleT $ map snd vs) p xs
  -- storeResult g ids vs
  run (iTbl' |+> iTbl) (tTbl' |+> tTbl) vs p $ do
    (ids,_) <- trans (TupleT $ map snd vs) xs
    storeResult ids
translate _ _ _ _                      = Left "Not a function"

-- |
-- typecheck は、グローバル関数の引数と返り値の数がグローバル変数と一致するか調べる
typecheck :: GlobalVariables -> LExp -> RExp -> Either String ()
typecheck vs (TupleL ls) (TupleR rs) = if n == length ls && n == length rs then return () else Left "mismatch the length of tuples"
  where n = length vs
typecheck _ _ _ = Left "must be a tuple to tuple function"

insertNode :: MonadState OMGraph m => OMInst -> TExp -> [Annot] -> m (Tree OMID, TExp)
insertNode i t as = do
  g <- get
  let omid = M.size g
      node = OMNode { inst = i
                    , theType = t
                    , annot = as
                    }
  put $! M.insert omid node g
  return (Leaf omid, t)


trans :: TExp -> RExp -> TransM (Tree OMID, TExp)
trans t (IdentR n) = do
  whenGlobal n (\t' -> insertNode (LoadGlobal (vec [0,0,0]) n) t' [SourceName n]) $ do
    v <- lookupIdent n
    b <- isManifest n
    let as = if b then [SourceName n, ManifestNode] else [SourceName n]
    (ids,t1) <- transValue t v
    updateAnnots as ids
    return (ids,t1)
trans t (ImmR x) =
  if not (isNumType t)
     then reportError $ "invalid type: " ++ show x ++ " is not " ++ show t
     else insertNode (Imm x) t []

transValue :: TExp -> (AlexPosn,[IdentName],Value) -> TransM (Tree OMID, TExp)
transValue t0 (p,idx,v) =
  case v of
    ValueR r     -> local (updateEnv p idx) $ trans t0 r
    ValueN ids t -> return (ids,t)
    ValueI i     -> insertNode (LoadIndex i) (IdentT "int") []

  where
    updateEnv p1 ns e = let iTbl = HM.fromList [(n,(p1,[],ValueI x)) | (n,x) <- zip idx [0..]]
                        in e { identTable = iTbl |+> identTable e
                             , sourcePos = p1
                             }

whenGlobal :: IdentName -> (TExp -> TransM a) -> TransM a -> TransM a
whenGlobal n act1 act2 = do
  vs <- reader gVariables
  if n `elem` (map fst vs)
     then act1 (fromJust $ lookup n vs)
     else act2

--trans :: IdentTable -> TypeTable -> GlobalVariables -> OMGraph -> TExp -> AlexPosn -> RExp -> Either String (OMGraph, Tree OMID, TExp)
--trans !iTbl !tTbl vs !g t _ (IdentR n)
--  | n `elem` (map fst vs) = do
--    let Just t' = lookup n vs
--    return (insertNode g (LoadGlobal (vec [0,0,0]) n) t' [SourceName n])
--  | otherwise = do
--    (p,idx,v) <- lookupIdent n iTbl
--    let as = if n `isManifest` tTbl then [SourceName n,ManifestNode] else [SourceName n]
--    updateAnnots as <$>
--      case v of
--        ValueR r -> let iTbl' = HM.fromList [(i,(p,[],ValueI x)) | (i,x) <- zip idx [0..]]
--                    in trans (iTbl <> iTbl') tTbl vs g t p r
--        ValueN ids t1 -> return (g,ids,t1)
--        ValueI i -> return $ insertNode g (LoadIndex i) (IdentT "int") []
--trans !iTbl !tTbl vs !g t p (ImmR x) =
--  if not (isNumType t)
--    then Left $ "invalid type: " ++ show x ++ " is not " ++ show t
--    else return (insertNode g (Imm x) t [])
--trans !iTbl !tTbl vs !g t p (TupleR xs)       =
--  case t of
--    TupleT ts -> transTuple iTbl tTbl vs g p (zip ts xs)
--    SomeType -> transTuple iTbl tTbl vs g p (map (\x -> (SomeType,x)) xs)
--    _ -> Left $ "invalid type: " ++ show (TupleR xs) ++ " is not " ++ show t
--trans !iTbl !tTbl vs !g t p (GridR idx r)     = do
--  (g1,ids1,t1) <- trans iTbl tTbl vs g t p r
--  case t1 of
--    (IdentT _)     -> return (g1,ids1,t1)
--    (GridT off t') -> do
--      let newPos = (-) <$> off <*> (fmap (\(NPlusK _ x) -> x) idx)
--          intOff = fmap floor newPos
--          newOff = (-) <$> newPos <*> (fmap fromIntegral intOff)
--          t2 = GridT newOff t'
--      if intOff == (pure [0,0,0])
--         then return (g1,ids1,t1)
--         else insertNode g1 (Load (fmap negate intOff) ids1) t1 []
--    (TupleT ts)    -> undefined
--trans !iTbl !tTbl vs !g t p (UniopR op r)     = do
--  -- TODO: 型のチェック
--  (g1,ids1,t1) <- trans iTbl tTbl vs g t p r
--  let (g2,ids2) = insertUniop op g1 ids1 t1
--  return (g2,ids2,t1)
--trans !iTbl !tTbl vs !g t p (BinopR op r1 r2) = do
--  -- TODO: 型のキャスト
--  (g1,ids1,t1) <- trans iTbl tTbl vs g t p r1
--  (g2,ids2,t2) <- trans iTbl tTbl vs g1 t p r2
--  matchType t1 t2 -- スカラーの拡張に対応できていない
--  let (g3,ids3,t3) = insertBinop op g2 ids1 ids2 t1
--  return (g3,ids3,t3)
--trans !iTbl !tTbl vs !g t p (LetR b r)        = do
--  iTbl' <- makeIdentTable b
--  tTbl' <- makeTypeTable b
--  trans (iTbl <> iTbl') (tTbl <> tTbl') vs g t p r
--trans !iTbl !tTbl vs !g t p (LambdaR args r)  = Left "error"
--trans !iTbl !tTbl vs !g t p (IfR cnd r1 r2)   = do
--  -- TODO: 型のキャスト
--  (g1,ids1,t1) <- trans iTbl tTbl vs g (IdentT "bool") p cnd
--  (g2,ids2,t2) <- trans iTbl tTbl vs g1 t p r1
--  (g3,ids3,t3) <- trans iTbl tTbl vs g2 t p r2
--  matchType t1 (IdentT "bool") -- 正しいふるまいではない
--  matchType t2 t3
--  let (g4,ids4) = insertIf g3 ids1 ids2 ids3 t2
--  return (g4,ids4,t2)
--trans !iTbl !tTbl vs !g t p (AppR r1 r2)      = do
---- r1 として許容できるのは
---- - IdentR
---- - LambdaR
---- - TupleR
---- のいずれか
----
----
---- 次のようなものは許容しない (v2.3 でも許容していない)
---- f = if ... then fun(x) ... else fun(x) ...
---- q = f a
--  case r1 of
--    -- TODO: 外部関数への対応
--    IdentR n    | n `isExternFunc` tTbl -> undefined
--                | otherwise -> do
--      (p1,_,r) <- lookupIdent n iTbl
--      case r of
--        ValueR r'     -> trans iTbl tTbl vs g t p1 (AppR r' r2)
--        ValueN (Node ids) (TupleT ts) -> do
--          i <- evalToInt r2 iTbl
--          if i >= 0 && i < length ts then return (g,ids !! i,ts !! i) else Left "tuple indexing is too large"
--        _      -> Left $ "error: " ++ show (T.unpack n) ++ "is not appliable"
--    TupleR xs   -> evalToInt r2 iTbl >>= (\i -> if i >= 0 && i < length xs then trans iTbl tTbl vs g t p (xs !! i) else Left "tuple indexing is too large")
--    LambdaR l r -> do
--      (g1,ids1,t1) <- trans iTbl tTbl vs g t p r2
--      iTbl' <- bindArgs p l ids1
--      trans (iTbl <> iTbl') tTbl vs g t p r
--    _ -> Left $ "error: " ++ show r1 ++ "is not appliable"

lookupIdent :: IdentName -> TransM (AlexPosn, [IdentName], Value)
lookupIdent n = do
  tbl <- reader identTable
  case HM.lookup n tbl of
    Nothing -> reportError $ "Not found the identifier " ++ show (T.unpack n)
    Just x  -> return x

evalToInt :: RExp -> IdentTable -> Either String Int
evalToInt (ImmR n) _ = if denominator n == 1 then return (fromInteger $ numerator n) else Left "non-integer indexing in tuple access"
evalToInt (IdentR n) tbl = do
  (_,_,v) <- lookupIdent n tbl
  case v of
    ValueR r -> evalToInt r tbl
    _        -> Left ""
evalToInt r _ = Left $ show r ++ " is not integer"

bindArgs :: AlexPosn -> LExp -> Tree OMID -> Either String IdentTable
bindArgs p (TupleL ls) (Node ids) | length ls == length ids = undefined
bindArgs _ _ _                    = Left "tuple length mismatch"

-- transTuple :: IdentTable -> TypeTable -> GlobalVariables -> OMGraph -> AlexPosn -> [(TExp,RExp)] -> Either String (OMGraph, Tree OMID, TExp)
-- transTuple iTbl tTbl vs g p xs = (\(g0,ids,ts) -> (g0,Node ids,TupleT ts)) <$> foldM worker (g,[],[]) xs
--   where
--     worker (g0,ids,ts) (t,r) = do
--       (g1,i1,t1) <- trans iTbl tTbl vs g0 t p r
--       return (g1,ids <> [i1], ts <> [t1])

zipTreeWithTExp :: Tree a -> TExp -> Tree (a,TExp)
zipTreeWithTExp (Leaf x) t = Leaf (x,t)
zipTreeWithTExp (Node xs) (TupleT ts) = Node [zipTreeWithTExp x t | (x,t) <- zip xs ts]
zipTreeWithTExp _ _ = error "error at Formura0.Middleend.Translate.zipTreeWithTExp"

foldTree :: (g -> a -> (g,b)) -> g -> Tree a -> (g, Tree b)
foldTree f g (Leaf x)      = Leaf <$> f g x
foldTree _ g (Node [])     = (g,Node [])
foldTree f g (Node (x:xs)) = (g2, Node (y:ys))
  where
    (g1,y) = foldTree f g x
    (g2,Node ys) = foldTree f g1 (Node xs)

-- insertUniop :: Op1 -> OMGraph -> Tree OMID -> TExp -> (OMGraph, Tree OMID)
-- insertUniop op g ids ts = foldTree worker g (zipTreeWithTExp ids ts)
--   where
--     worker g0 (i,t) = (\(g',Leaf i',_) -> (g',i')) $ insertNode g0 (Uniop op i) t []

-- |
-- 二項演算のふるまい
-- (a,b) + (c,d) => (a+c,b+d)
-- a + (b,c) => (a+b,a+c)
-- (a,b) + c => (a+c,b+c)
--
insertBinop :: Op2 -> OMGraph -> Tree OMID -> Tree OMID -> TExp -> (OMGraph, Tree OMID, TExp)
insertBinop = undefined

-- |
-- 現在 (v2.3) の振る舞いでは
-- if (a,b) == (1,2) then (0,10) else (1,11)
-- が
-- (if a == 1 then 0 else 1, if b == 2 then 10 else 11)
-- と同値になる
--
-- また
-- if a == 1 then (0,10) else (1,11)
-- が
-- (if a == 1 then 0 else 1, if a == 1 then 10 else 11)
-- と同値になる
--
insertIf :: OMGraph -> Tree OMID -> Tree OMID -> Tree OMID -> TExp -> (OMGraph, Tree OMID)
insertIf = undefined

storeResult :: Tree OMID -> TransM ()
storeResult ids = do
  vs <- map fst <$> reader gVariables
  zipWithM_ (\i v -> insertNode (Store v i) (IdentT "void") []) (flatten ids) vs

isNumType :: TExp -> Bool
isNumType (IdentT t)  = t `notElem` ["bool", "string"]
isNumType (TupleT _)  = False
isNumType (GridT _ t) = isNumType t
isNumType SomeType    = False

isManifest :: MonadReader Env m => IdentName -> m Bool
isManifest n = do
  tbl <- reader typeTable
  return $ case HM.lookup n tbl of
    Just (ms,_) -> TMManifest `elem` ms
    Nothing     -> False

isExternFunc :: IdentName -> TypeTable -> Bool
isExternFunc n tbl = case HM.lookup n tbl of
                       Just (ms,_) -> TMExtern `elem` ms
                      -- ^ FIXME: ほんとは型もチェックするべき
                       Nothing     -> False

updateAnnots :: MonadState OMGraph m => [Annot] -> Tree OMID -> m ()
updateAnnots as ids = mapM_ addAnnots (flatten ids)
  where
    addAnnots i = modify' (\g -> M.adjust (\n -> n { annot = as }) i g)

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
