{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
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
           -- ^ グルーバルな変数定義やローカルな変数定義の識別子の値
           | ValueI !Int
           -- ^ グリッド型の識別子の値
           | ValueN (Tree (OMID,TExp))
           -- ^ 関数の仮引数の識別子の値
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
  deriving (Eq,Show,Functor,Foldable,Traversable)

flatten :: Tree a -> [a]
flatten (Leaf a)  = [a]
flatten (Node ts) = concatMap flatten ts

zipWithTreeM :: MonadError TransError m => (a -> b -> m c) -> Tree a -> Tree b -> m (Tree c)
zipWithTreeM f (Leaf x) (Leaf y)    = Leaf <$> f x y
zipWithTreeM f x@(Leaf _) (Node ys) = Node <$> mapM (\y -> zipWithTreeM f x y) ys
zipWithTreeM f (Node xs) y@(Leaf _) = Node <$> mapM (\x -> zipWithTreeM f x y) xs
zipWithTreeM f (Node xs) (Node ys) | length xs == length ys = Node <$> zipWithM (zipWithTreeM f) xs ys
                                   | otherwise = throwError "tree mismatch"

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
--
-- - グローバルな IdentTable と TypeTable を構築する
-- - グローバル変数を特定する
-- - init, step, first_step, filter 関数についてグラフを構築する
-- - 特殊宣言を処理する
-- - numerical config の検証と変換をする
genOMProgram :: Program -> NumericalConfig -> Either String OMProgram
genOMProgram prog cfg = do
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

makeIdentTable :: MonadError TransError m => Program -> m IdentTable
makeIdentTable prog = return $ HM.fromList [ (n,(p,idx,ValueR e)) | VarDecl p l r <- prog, (n,!idx,e) <- unwrap l r]
  where
    unwrap (TupleL xs) r = concat [unwrap x (AppR r (ImmR i)) | (x,i) <- zip xs [0..]]
    unwrap (IdentL n) r  = [(n,[],r)]
    unwrap (GridL (Vec (ZipList idx)) l) r = [(n,idx++idx',r') | (n,idx',r') <- unwrap l r]

makeTypeTable :: MonadError TransError m => Program -> m TypeTable
makeTypeTable prog = return $ HM.fromList [ (n,(ms,t')) | (TypeDecl _ (ModifiedType ms t) l) <- prog, (n,t') <- unwrap l t]
  where
    unwrap (IdentL n) t            = [(n,t)]
    unwrap (GridL _ l) t           = unwrap l t
    unwrap (TupleL xs) (TupleT ts) = concat [unwrap x t | (x,t) <- zip xs ts]
    unwrap (TupleL xs) SomeType    = concat [unwrap x SomeType | x <- xs]
    unwrap _ _                     = error "error" -- FIX ME

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
    typeOf n tTbl = case HM.lookup n tTbl of
                      Nothing -> Left $ "Not found " ++ show (T.unpack n)
                      Just t  -> Right (snd t)

    unwrap xs = case [n | (IdentR n) <- xs] of
                  ys | length ys == length xs -> Right ys
                     | otherwise -> Left "init must return a tuple of grid"

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
-- translate の仕様
--
-- ターゲットの関数のローカルな IdentTable と TypeTable を構築して、
-- trans 関数と storeResult 関数でグラフを構築する
translate :: IdentTable -> TypeTable -> GlobalVariables -> (AlexPosn,RExp) -> Either String OMGraph
translate iTbl tTbl vs (p,LambdaR args (LetR b xs)) = do
  typecheck vs args xs
  iTbl' <- makeIdentTable b
  tTbl' <- makeTypeTable b
  run (iTbl' |+> iTbl) (tTbl' |+> tTbl) vs p $ do
    res <- trans (TupleT $ map snd vs) xs
    storeResult res
translate _ _ _ _                      = Left "Not a function"

-- |
-- typecheck は、グローバル関数の引数と返り値の数がグローバル変数と一致するか調べる
--
-- FIXME: init 関数に対して正しく動作しない
typecheck :: GlobalVariables -> [LExp] -> RExp -> Either String ()
typecheck vs ls (TupleR rs) = if n == length ls && n == length rs then return () else Left "mismatch the length of tuples"
  where n = length vs
typecheck _ _ _ = Left "must be a tuple to tuple function"

-- |
-- TransM の状態である OMGraph に変更を行うのは insertNode 関数のみ
insertNode :: MonadState OMGraph m => OMInst -> TExp -> [Annot] -> m (OMID, TExp)
insertNode i t as = do
  g <- get
  let omid = M.size g
      node = OMNode { inst = i
                    , theType = t
                    , annot = as
                    }
  put $! M.insert omid node g
  return $ (omid, t)


-- |
-- trans の仕様
--
-- trans は構文木をたどり OMGraph を構築する
--
-- OMGraph は、グローバル変数と即値からはじまり、グローバル変数でおわる。
-- trans は、結果のグローバル変数から構文木をたどっていき、最終的にはグローバル変数か即値にいきつく。
-- trans は次のような仕事を行う。
-- - 型チェック
-- - タプルの展開
-- - 関数の展開 (インライン化)
--
-- trans の第一引数は、期待する型である。
-- これは、タプルの展開と即値の型を決定するのに使う。
-- 期待される型は、グローバル変数の型に由来するが、
-- - 識別子をたどる (IdentR)
-- - 条件分岐の条件式部分
-- - 関数の実引数
-- において SomeType に変化しうる。
-- trans の返り値にある TExp は決定されたノードの型である。
-- 期待する型には SomeType が含まれるが、決定されたノードの型には SomeType は含まれない。
-- また、Tree a 型がタプルの構造を保存するため、決定されたノード型は IdentT か GridT である (はず)。
trans :: TExp -> RExp -> TransM (Tree (OMID, TExp))
trans t (IdentR n) = do
  vs <- reader gVariables
  if n `elem` (map fst vs)
     then Leaf <$> insertNode (LoadGlobal (vec [0,0,0]) n) (fromJust $ lookup n vs) [SourceName n]
     else do
       v <- lookupIdent n
       t' <- lookupType n
       t0 <- expectType t t'
       res <- transValue t0 v
       updateAnnots n res
       return res
trans SomeType (ImmR x) = Leaf <$> insertNode (Imm x) (if denominator x == 1 then IdentT "int" else IdentT "float") [] -- FIXME: とりあえずの実装
trans t (ImmR x) =
  if not (isNumType t)
     then reportError $ "invalid type: " ++ show x ++ " is not " ++ show t
     else Leaf <$> insertNode (Imm x) t []
trans t (TupleR xs) =
  Node <$> case t of
    TupleT ts | length xs == length ts -> zipWithM trans ts xs
    SomeType -> mapM (trans SomeType) xs
    _ -> reportError $ "invalid type: " ++ show (TupleR xs) ++ " is not " ++ show t
trans t (GridR idx r) = trans t r >>= mapM (transGrid idx)
trans t (UniopR op r) = trans t r >>= mapM (transUniop op)
trans t (BinopR op r1 r2) = do
  x1 <- trans t r1
  x2 <- trans t r2
  transBinop op x1 x2
trans t (LetR b r) = do
  iTbl <- makeIdentTable b
  tTbl <- makeTypeTable b
  local (\e -> e { identTable = iTbl |+> identTable e, typeTable = tTbl |+> typeTable e })  $ trans t r
trans _ r@(LambdaR _ _) = reportError $ "invalid value: " ++ show r ++ " is a function"
trans t (IfR r1 r2 r3) = do
  x1 <- trans SomeType r1
  x2 <- trans t r2
  x3 <- trans t r3
  transIf x1 x2 x3
trans t (AppR r1 r2) =
-- r1 として許容できるのは
-- - IdentR
-- - LambdaR
-- - TupleR
-- のいずれか
--
--
-- 次のようなものは許容しない (v2.3 でも許容していない)
-- f = if ... then fun(x) ... else fun(x) ...
-- q = f a
--
  case r1 of
    IdentR n -> do
      b <- isExternFunc n
      if b
        then transExternFunc n r2
        else do
          (p1,_,r) <- lookupIdent n
          case r of
            ValueR r'        -> local (\e -> e { sourcePos = p1 }) $ trans t (AppR r' r2)
            ValueN (Node xs) -> evalToInt r2 >>= nthOfTuple xs
            _                -> reportError $ "invalid type: " ++ show (T.unpack n) ++ " is not appliable"
    TupleR xs   -> evalToInt r2 >>= nthOfTuple xs >>= trans t
    LambdaR l r -> do
      iTbl <- bindArgs l r2
      local (\e -> e { identTable = iTbl |+> identTable e }) $ trans t r
    _ -> reportError $ "invalid type: " ++ show r1 ++ " is not appliable"

  where
    nthOfTuple xs i = if i >= 0 && i < length xs then return (xs !! i) else reportError $ "out-of-range tuple index: " ++ show i

transValue :: TExp -> (AlexPosn,[IdentName],Value) -> TransM (Tree (OMID,TExp))
transValue t0 (p,idx,v) =
  case v of
    ValueR r  -> local (updateEnv p idx) $ trans t0 r
    ValueN xs -> return xs
    ValueI i  -> Leaf <$> insertNode (LoadIndex i) (IdentT "int") []

  where
    updateEnv p1 ns e = let iTbl = HM.fromList [(n,(p1,[],ValueI x)) | (n,x) <- zip ns [0..]]
                        in e { identTable = iTbl |+> identTable e
                             , sourcePos = p1
                             }

transGrid :: Vec NPlusK -> (OMID,TExp) -> TransM (OMID,TExp)
transGrid npk res@(i,t) =
  case t of
    IdentT _ -> return res
    GridT off t' -> do
      let newPos = (-) <$> off <*> (fmap (\(NPlusK _ x) -> x) npk)
          intOff = fmap floor newPos
          newOff = (-) <$> newPos <*> (fmap fromIntegral intOff)
          t1 = GridT newOff t'
      if intOff == (vec [0,0,0])
         then return $ (i,t1)
         else insertNode (Load (fmap negate intOff) i) t1 []
    _ -> reportError "bug in transGrid"

transUniop :: Op1 -> (OMID,TExp) -> TransM (OMID,TExp)
transUniop op (i,t) | isNumType t = insertNode (Uniop op i) t []
                    | otherwise = reportError $ "invalid type: " ++ show t ++ " is not numeric type"

transBinop :: Op2 -> Tree (OMID,TExp) -> Tree (OMID,TExp) -> TransM (Tree (OMID,TExp))
transBinop op x1 x2 = zipWithTreeM (\(i1,t1) (i2,t2) -> matchType t1 t2 >>= inferType op >>= (\t3 -> insertNode (Binop op i1 i2) t3 [])) x1 x2

-- if の仕様
--
-- ここでは、各識別子 (x, a1, b1...) はスカラーであるとする。
-- if x then (a1,a2) else (b1,b2) #=> (if x then a1 else b1, if x then a2 else b2)
-- if (x1,x2) then (a1,a2) else (b1,b2) #=> (if x1 then a1 else b1, if x2 then a2 else b2)
transIf :: Tree (OMID,TExp) -> Tree (OMID,TExp) -> Tree (OMID,TExp) -> TransM (Tree (OMID,TExp))
transIf x1 x2 x3 = do
  res <- zipWithTreeM (\(i2,t2) (i3,t3) -> if t2 == t3 then return ((i2,i3),t2) else reportError $ "type mismatch: " ++ show t2 ++ " /= " ++ show t3 ) x2 x3
  zipWithTreeM (\(i1,t1) ((i2,i3),t2) -> if isBoolishType t1 then insertNode (If i1 i2 i3) t2 [] else reportError $ "type mismatch: " ++ show t1 ++ " is not boolish type") x1 res

-- |
-- transExternFunc の仕様
--
-- 将来、この仕様は変更されるだろう
--
-- v2.3 までは、スカラー関数のみを扱ってきた
-- そのふるまいを継承する
transExternFunc :: IdentName -> RExp -> TransM (Tree (OMID,TExp))
transExternFunc fn r = do
  res <- trans SomeType r
  mapM (\(i,t) -> insertNode (Call1 fn [i]) t []) res

lookupIdent :: IdentName -> TransM (AlexPosn, [IdentName], Value)
lookupIdent n = do
  tbl <- reader identTable
  case HM.lookup n tbl of
    Nothing -> reportError $ "Not found the identifier " ++ show (T.unpack n)
    Just x  -> return x

lookupType :: IdentName -> TransM TExp
lookupType n = do
  tbl <- reader typeTable
  return $ case HM.lookup n tbl of
    Nothing    -> SomeType
    Just (_,t) -> t

evalToInt :: RExp -> TransM Int
evalToInt (ImmR n) = if denominator n == 1 then return (fromInteger $ numerator n) else reportError "non-integer indexing in tuple access"
evalToInt (IdentR n) = do
  (p,_,v) <- lookupIdent n
  case v of
    ValueR r -> local (\e -> e { sourcePos = p }) $ evalToInt r
    _        -> reportError $ show (T.unpack n) ++ " is not integer"
evalToInt r = reportError $ show r ++ " is not integer"

-- |
-- bindArgs の仕様
--
-- l が識別子なら、そのままマッチ可能
-- l と r2 が同じ長さのタプルなら、そのままマッチ可能
-- l がタプルで、 r2 がタプル以外なら、r2 を評価してからマッチを行う
--
-- あらゆる場合において r2 を先に評価しないのは、関数の受け渡しを許容するためである
-- (LambdaR に対する trans は常に失敗する)
bindArgs :: [LExp] -> RExp -> TransM IdentTable
bindArgs [IdentL l] r = do
  p <- reader sourcePos
  return $ HM.fromList [(l,(p,[],ValueR r))]
bindArgs ls r@(TupleR rs) = do
  if length ls /= length rs
    then reportError "tuple length mismatch"
    else do
      p <- reader sourcePos
      makeIdentTable [VarDecl p (TupleL ls) r]
bindArgs ls r = do
  res <- trans SomeType r
  p <- reader sourcePos
  makeIdentTable' p (TupleL ls) res

  where
    -- makeIdentTable' の LExp に GridL は絶対にない (Parser の構成から)
    makeIdentTable' :: AlexPosn -> LExp -> Tree (OMID,TExp) -> TransM IdentTable
    makeIdentTable' p0 (TupleL ls0) (Node xs) | length ls == length xs = HM.unions <$> zipWithM (makeIdentTable' p0) ls0 xs
    makeIdentTable' p0 (IdentL l) x = return $ HM.singleton l (p0,[],ValueN x)
    makeIdentTable' _ _ _ = reportError "tuple length mismatch"

storeResult :: Tree (OMID,TExp) -> TransM ()
storeResult res = do
  vs <- map fst <$> reader gVariables
  -- FIXME: 変数名に _next を付与
  zipWithM_ (\(i,_) v -> insertNode (Store v i) (IdentT "void") []) (flatten res) vs

isNumType :: TExp -> Bool
isNumType (IdentT t)  = t `notElem` ["bool", "string"]
isNumType (TupleT _)  = False
isNumType (GridT _ t) = isNumType t
isNumType SomeType    = False

isBoolishType :: TExp -> Bool
isBoolishType (IdentT t)  = t == "bool"
isBoolishType (TupleT _)  = False
isBoolishType (GridT _ t) = isBoolishType t
isBoolishType SomeType    = False

isManifest :: MonadReader Env m => IdentName -> m Bool
isManifest n = do
  tbl <- reader typeTable
  return $ case HM.lookup n tbl of
    Just (ms,_) -> TMManifest `elem` ms
    Nothing     -> False

isExternFunc :: MonadReader Env m => IdentName -> m Bool
isExternFunc n = do
  tbl <- reader typeTable
  return $ case HM.lookup n tbl of
    Just (ms,_) -> TMExtern `elem` ms
    -- ^ FIXME: ほんとは型もチェックするべき
    Nothing     -> False

updateAnnots :: IdentName -> Tree (OMID,TExp) -> TransM ()
updateAnnots n ids = do
  b <- isManifest n
  let as = if b then [SourceName n, ManifestNode] else [SourceName n]
  mapM_ (addAnnots as) (flatten ids)
  where
    addAnnots as (i,_) = modify' (\g -> M.adjust (\node -> node { annot = as }) i g)

expectType :: TExp -> TExp -> TransM TExp
expectType t1 SomeType = return t1
expectType SomeType t2 = return t2
expectType (TupleT ts1) (TupleT ts2) | length ts1 == length ts2 = TupleT <$> zipWithM expectType ts1 ts2
expectType t1 t2       = matchType t1 t2

matchType :: TExp -> TExp -> TransM TExp
matchType (IdentT t1) (IdentT t2) | t1 == t2 = return $ IdentT t1
                                  | match ["float","double"] = return $ IdentT "double"
                                  | match ["int","double"] = return $ IdentT "double"
                                  | match ["int","float"] = return $ IdentT "float"
  where match ts = t1 `elem` ts && t2 `elem` ts
matchType (GridT npk1 t1) t2@(IdentT _) = GridT npk1 <$> matchType t1 t2
matchType t1@(IdentT _) (GridT npk2 t2) = GridT npk2 <$> matchType t1 t2
matchType (GridT npk1 t1) (GridT npk2 t2) | npk1 == npk2 = GridT npk1 <$> matchType t1 t2
matchType t1 t2 = reportError $ "type mismatch: " ++ show t1 ++ " /= " ++ show t2

inferType :: Op2 -> TExp -> TransM TExp
inferType op t | isArith op = if isNumType t then return t else reportError $ "invalid type: " ++ show t ++ " is not numeric type"
               | otherwise = case t of
                               IdentT _    -> return $ IdentT "bool"
                               GridT npk _ -> return $ GridT npk (IdentT "bool")
                               _           -> reportError "bug in inferType"
  where
    isArith o = o `elem` [Add,Sub,Mul,Div,Pow]
