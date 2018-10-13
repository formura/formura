{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
module Formura.OrthotopeMachine.Translate where

import           Algebra.Lattice
import           Control.Applicative
import           Control.Exception
import           Control.Lens hiding (at, op)
import           Control.Monad
import "mtl"     Control.Monad.Reader hiding (fix)
import           Data.Foldable
import qualified Data.Map as M
import           Data.Ratio
import qualified Data.Set as S
import           System.Exit
import           Text.Trifecta (failed, raiseErr)

import qualified Formura.Annotation as A
import           Formura.Annotation.Representation
import           Formura.Compiler
import           Formura.GlobalEnvironment
import           Formura.Language.Combinator
import           Formura.Language.TExpr
import           Formura.NumericalConfig
import           Formura.OrthotopeMachine.Graph
import           Formura.Syntax
import           Formura.Type
import           Formura.Vec

type Binding = M.Map IdentName ValueExpr
type LexBinding = M.Map IdentName ValueLexExpr

class HasBinding s where
  binding :: Lens' s Binding

instance HasBinding Binding where
  binding = simple

data CodegenState = CodegenState
  { _codegenSyntacticState :: CompilerSyntacticState
  , _codegenGlobalEnvironment :: GlobalEnvironment
  , _theGraph :: OMGraph
  }

makeClassy ''CodegenState

instance HasGlobalEnvironment CodegenState where
  globalEnvironment = codegenGlobalEnvironment

instance HasCompilerSyntacticState CodegenState where
  compilerSyntacticState = codegenSyntacticState

defaultCodegenState :: CodegenState
defaultCodegenState = CodegenState
  { _codegenSyntacticState = defaultCompilerSyntacticState {_compilerStage = "codegen"}
  , _theGraph = M.empty
  , _codegenGlobalEnvironment = defaultGlobalEnvironment
  }

defaultCodegenRead :: Binding
defaultCodegenRead = M.empty

-- | the code generator monad.
type GenM = CompilerMonad Binding () CodegenState
type LexGenM = CompilerMonad LexBinding () CodegenState


-- | Set up the 'GlobalEnvironment' from the 'SpecialDeclaration' part of the the 'Program' .

setupGlobalEnvironment :: Program -> GenM ()
setupGlobalEnvironment prog = do
  let defaultGridStructTypeName = "Formura_Grid_Struct"
      defaultGridStructInstanceName = "formura_data"
  dim <- case concatMap findDimension spDecls of
    [n] -> return n
    [] -> raiseErr $ failed "no dimension declaration found."
    _  -> raiseErr $ failed "multiple dimension declaration found."
  axs <- case concatMap findAxesDeclaration spDecls of
    [xs] | length xs == dim -> return xs
    [_] -> raiseErr $ failed "number of declared axes does not match the declared dimension."
    [] -> raiseErr $ failed "no axes declaration found."
    _  -> raiseErr $ failed "multiple axes declaration found."
  gridType <- case concatMap findGridStructTypeNameDeclaration spDecls of
    [t] -> return (if null t then defaultGridStructTypeName else t)
    [] -> return defaultGridStructTypeName
    _ -> raiseErr $ failed "multiple grid_struct_type_name found."
  gridInstance <- case concatMap findGridStructInstanceNameDeclaration spDecls of
    [n] -> return (if null n then defaultGridStructInstanceName else n)
    [] -> return defaultGridStructInstanceName
    _ -> raiseErr $ failed "multiple grid_struct_instance_name found."
  dimension .= dim
  axesNames .= axs
  gridStructTypeName .= gridType
  gridStructInstanceName .= gridInstance
  let bases | dim == 1 = [[1]]
            | dim == 2 = [[1,0],[0,1],[1,1]]
            | dim == 3 = [[1,0,0],[0,1,0],[0,0,1],[1,1,0],[1,0,1],[0,1,1],[1,1,1]]
            | otherwise = error "Not support"
  commBases .= bases
  where
    spDecls = prog ^. programSpecialDeclarations

    findDimension :: SpecialDeclaration -> [Int]
    findDimension (DimensionDeclaration n) = [n]
    findDimension _ = []

    findAxesDeclaration :: SpecialDeclaration -> [[IdentName]]
    findAxesDeclaration (AxesDeclaration xs) = [xs]
    findAxesDeclaration _ = []

    findGridStructTypeNameDeclaration :: SpecialDeclaration -> [IdentName]
    findGridStructTypeNameDeclaration (GridStructTypeNameDeclaration t) = [t]
    findGridStructTypeNameDeclaration _ = []

    findGridStructInstanceNameDeclaration :: SpecialDeclaration -> [IdentName]
    findGridStructInstanceNameDeclaration (GridStructInstanceNameDeclaration n) = [n]
    findGridStructInstanceNameDeclaration _ = []



class Generatable f where
  gen :: f (GenM ValueExpr) -> GenM ValueExpr

freeNodeID :: GenM OMNodeID
freeNodeID = do
  g <- use theGraph
  return $ fromIntegral $ M.size g

insert :: OMInstruction -> OMNodeType -> GenM ValueExpr
insert inst typ = do
  n0 <- freeNodeID
  foc <- use compilerFocus
  let a = case foc of
        Just meta -> A.singleton meta
        Nothing   -> A.empty
  theGraph %= M.insert n0 (Node inst typ a)
  mmeta <- use compilerFocus
  case mmeta of
    Just meta -> theGraph . ix n0 . A.annotation %= A.set meta
    _         -> return ()

  return $ NodeValue n0 typ


-- | Find the type of a 'ValueExpr' .
typeOfVal :: ValueExpr -> TypeExpr
typeOfVal (Imm _)         = ElemType "Rational"
typeOfVal (NodeValue _ t) = subFix t
typeOfVal (FunValue _ _)  = FunType
typeOfVal (Tuple xs)      = Tuple $ map typeOfVal xs
typeOfVal _ = error "no match (Formura.OrthotopeMachine.Translate.typeOfVal)"


-- | convert a value to other value, so that the result may have the given type
castVal :: TypeExpr -> ValueExpr -> GenM ValueExpr
castVal t1 vx = let t0 = typeOfVal vx in case (t1, t0, vx) of
  _ | t1 == t0 -> return vx
  (ElemType _, ElemType _, _) -> return vx
  (GridType vec (ElemType te), ElemType _, n :. _) -> return (n :. GridType vec (ElemType te))
  (GridType vec0 _, GridType vec1 _, _) | vec0 == vec1 ->  return vx
  _ -> raiseErr $ failed $ "cannot convert type " ++ show t0 ++ " to " ++ show t1


instance Generatable ImmF where
  gen (Imm r) = insert (Imm r) (ElemType "Rational")
  gen _ = error "no match (Formura.OrthotopeMachine.Translate.gen)"

spoonTExpr :: (TupleF ∈ fs) => TExpr (Lang fs) -> GenM (Lang fs)
spoonTExpr x = case x ^? tExpr of
  Nothing -> raiseErr $ failed "Tuple length mismatch"
  Just y -> return y

instance Generatable OperatorF where
  gen (Uniop op gA)       = do a <- gA                  ; goUniop op a
  gen (Binop op gA gB)    = do a <- gA; b <- gB         ; goBinop op a b
  gen (Triop op gA gB gC) = do
    a <- gA; b <- gB; c <- gC;
    ret <- sequence $ goTriop op <$> tExpr # a <*> tExpr # b <*> tExpr # c
    spoonTExpr ret
  gen (Naryop op gXs) = do
    xs <- sequence gXs
    let list_texpr_vals = map (tExpr#) xs :: [TExpr ValueExpr]
        texpr_list_vals = sequenceA list_texpr_vals :: TExpr [ValueExpr]
    ret <- sequence $ goNaryop op <$> texpr_list_vals
    spoonTExpr ret
  gen _ = error "no match (Formura.OrthotopeMachine.Translate.gen)"


type MVsT = Maybe ([OMNodeID], OMNodeType)

goNaryop :: IdentName -> [ValueExpr] -> GenM ValueExpr
goNaryop op xs = do
  mvst <- foldrM foldVT Nothing xs
  case mvst of
    Nothing -> raiseErr $ failed "unexpected N-ary operator with 0 argument"
    Just (vs,t) -> insert (Naryop op vs) t

  where
    foldVT :: ValueExpr -> MVsT -> GenM MVsT
    foldVT (av :. at) Nothing = return $ Just ([av], at)
    foldVT (av :. at) (Just (bvs,bt)) = case at /\ bt of
      TopType -> raiseErr $ failed $ unwords
             ["there is no common type that can accomodate both hand side:", op, show at , show bt]
      ct -> return $ Just (av:bvs, ct)
    foldVT _ _ = raiseErr $ failed "unexpected path in N-ary operator"

goUniop :: IdentName -> ValueExpr -> GenM ValueExpr
goUniop op (av :. at) = insert (Uniop op av) at
goUniop op (f@(FunValue _ _)) =
  return $ FunValue (Ident "x") (Uniop op (Apply (subFix f) (Ident "x")))
goUniop op (Tuple xs) = do
  vs <- traverse (goUniop op) xs
  return $ Tuple vs
goUniop _ _  = raiseErr $ failed "unexpected path in unary operator"

goBinop :: IdentName -> ValueExpr -> ValueExpr -> GenM ValueExpr
goBinop (".") a b = return $ FunValue (Ident "x") (Apply (subFix a) (Apply (subFix b) (Ident "x")))
goBinop op ax@(_ :. at) bx@(_ :. bt) = case at /\ bt of
  TopType -> raiseErr $ failed $ unwords
             ["there is no common type that can accomodate both hand side:", show at, op , show bt]
  ct -> do
    let typeModifier
          | op `S.member` comparisonOperatorNames = mapElemType (const "bool")
          | otherwise = id
    (av2 :. _) <- castVal (subFix ct) ax
    (bv2 :. _) <- castVal (subFix ct) bx
    insert (Binop op av2 bv2) (typeModifier ct)

goBinop op (f@(FunValue _ _)) (g@(FunValue _ _)) =
  return $ FunValue (Ident "x") (Binop op (Apply (subFix f) (Ident "x")) (Apply (subFix g) (Ident "x")))
goBinop op (f@(FunValue _ _)) (x@(_ :. _)) =
  return $ FunValue (Ident "x") (Binop op (Apply (subFix f) (Ident "x")) (subFix x))
goBinop op (x@(_ :. _)) (g@(FunValue _ _)) =
  return $ FunValue (Ident "x") (Binop op (subFix x) (Apply (subFix g) (Ident "x")))
goBinop op (Tuple xs) (Tuple ys) | length xs == length ys = do
                                     zs <- zipWithM (goBinop op) xs ys
                                     return $ Tuple zs
goBinop _ (Tuple _) (Tuple _) = raiseErr $ failed "tuple length mismatch."
goBinop op (x@(_ :. _)) (Tuple ys) = Tuple <$> sequence [goBinop op x y | y <- ys]
goBinop op (Tuple xs) (y@(_ :. _)) = Tuple <$> sequence [goBinop op x y | x <- xs]
goBinop o a b  = raiseErr $ failed $ unlines ["unexpected path in binary operator: ", "OP:",  show o,"LHS:",show a,"RHS:",show b]

isBoolishType :: OMNodeType -> Bool
isBoolishType (ElemType "bool") = True
isBoolishType (GridType _ x) = isBoolishType x
isBoolishType _ = False

goTriop :: IdentName -> ValueExpr -> ValueExpr -> ValueExpr -> GenM ValueExpr
goTriop op (av :. at) (bv :. bt) (cv :. ct)
  | op == "ite" = do
      unless (isBoolishType at) $
        raiseErr $ failed "the first argument of if-expr must be of type bool"
      let bct =  bt /\ ct
      case at /\ bct of
        TopType -> raiseErr $ failed $ unwords
                   ["Type mismatch in if-then-else expr:", show at, show bt, show ct]
        _ -> insert (Triop op av bv cv) bct
goTriop op _ _ _ = raiseErr $ failed $ "unexpected path in trinary operator" ++ show op

instance Generatable IdentF where
  gen (Ident n) = do
    b <- view binding
    case M.lookup n b of
      Nothing ->
        raiseErr $ failed $ "undefined variable: " ++ n ++ "\n Bindings:\n" ++ show b
      Just x  -> return $ subFix x
  gen _ = error "no match (Formura.OrthotopeMachine.Translate.gen)"


instance Generatable TupleF where
  gen (Tuple xsGen) = do
    xs <- sequence xsGen
    return $ Tuple xs
  gen _ = error "no match (Formura.OrthotopeMachine.Translate.gen)"

instance Generatable GridF where
  gen (Grid npks gen0) = do
    vex <- gen0
    case vex of
      vt0@(val0 :. typ0) -> case typ0 of
        ElemType _   -> return vt0
        GridType offs0 etyp0 -> do
          let
              patK   = fmap (^. _2) (npks :: Vec NPlusK)
              newPos = offs0 - patK
              intOff = fmap floor newPos
              newOff = liftA2 (\r n -> r - fromIntegral n) newPos intOff
              typ1 = GridType newOff etyp0

          if intOff == 0
                  then return (val0 :. typ1)
                  else insert (Shift (negate intOff) val0) typ1
        _ -> error "no match"
      Tuple vs -> do
        xs <- sequence [gen $ GridF npks (return v :: GenM ValueExpr) | v <- vs]
        return $ Tuple xs

      _ -> do
        b0 <- view binding
        liftIO $ mapM print $ M.toList b0
        raiseErr $ failed $ "unexpected pattern in gen of grid: " ++ show vex
  gen _ = raiseErr $ failed "unexpected happened in gen of grid"

instance Generatable ApplyF where
  gen (Apply fgen agen) = do
    f0 <- fgen
    a0 <- agen
    goApply f0 a0
  gen _ = error "no match (Formura.OrthotopeMachine.Translate.gen)"

evalToImm :: ValueExpr -> GenM (Maybe Rational)
evalToImm x = do
  g <- use theGraph
  case x of
    Imm r -> return $ Just r
    n :. _ -> case M.lookup n g of
      Just (Node (Imm r) _ _) -> return $ Just r
      _ -> error "no match"
    _ -> return Nothing

goApply :: ValueExpr -> ValueExpr -> GenM ValueExpr
goApply (Tuple xs) (Imm r) = do
  when (denominator r /= 1) $ raiseErr $ failed "non-integer indexing in tuple access"
  let n = fromInteger $  numerator r
      l = length xs
  when (n < 0 || n >= l) $ raiseErr $ failed "tuple access out of bounds"
  return $ xs!!n
goApply x@(Tuple _) arg0 = do
  i <- evalToImm arg0
  case i of
    Just r -> goApply x (Imm r)
    _ -> do
      g <- use theGraph
      raiseErr $ failed $ "tuple applied to non-constant integer: " ++ show arg0 ++ show g
goApply (FunValue l r) x = do
  lrs <- matchToLhs l x
  let x2 :: Binding
      x2 = M.fromList lrs
  local (M.union x2) $ genRhs r
goApply  _ _ = raiseErr $ failed "unexpected combination of application"

instance Generatable LambdaF where
  -- Expand all but bound variables, in order to implement lexical scope
  gen (Lambda l r) = do
    let conv :: Binding -> CodegenState -> (LexBinding, CodegenState)
        conv b s = (M.union (lexicalScopeHolder l) $ M.map subFix b, s)
    r' <- withCompiler conv $ resolveLex $ subFix r
    return $ FunValue l r'
  gen _ = error "no match (Formura.OrthotopeMachine.Translate.gen)"

-- resolveLex :: RXExpr -> LexGenM RXExpr
-- resolveLex r = compilerMFold resolveLexAlg r

resolveLex :: RXExpr -> LexGenM RXExpr
resolveLex (Ident n) = do
  b <- ask
  case M.lookup n b of
    Nothing -> raiseErr $ failed $ "undefined variable: " ++ n ++ "\nwhen resolving lexical binding." ++ "\n Bindings:\n" ++ unwords (M.keys b)
    Just x  -> return $ subFix x
resolveLex (Lambda l r) = do
  r' <- local (M.union (lexicalScopeHolder l)) $ resolveLex $ subFix r
  return $ FunValue l r'
resolveLex (FunValue l r) = do
  r' <- local (M.union (lexicalScopeHolder l)) $ resolveLex r
  return $ FunValue l r'
resolveLex (Let b r) = do
  let ls = map fst $ substs b :: [LExpr]
  r' <- local (M.union (M.unions $ map lexicalScopeHolder ls)) $ resolveLex r
  return $ Let b r'
resolveLex (In meta fx) = do
  con <- traverse resolveLex fx
  return $ In meta con

instance Generatable LetF where
  gen (Let b genX) = withBindings b genX
  gen _ = error "no match (Formura.OrthotopeMachine.Translate.gen)"

namesOfLhs :: LExpr -> TupleOfIdents
namesOfLhs (Ident n) = Ident n
namesOfLhs (Grid _ x) = namesOfLhs x
namesOfLhs (Vector _ x) = namesOfLhs x
namesOfLhs (Tuple xs) = Tuple $ map namesOfLhs xs
namesOfLhs _ = error "no match (Formura.OrthotopeMachine.Translate.namesOfLhs)"

indexNamesOfLhs :: LExpr -> Vec (Maybe IdentName)
indexNamesOfLhs (Grid npks _) = fmap indexNameOfNPK npks
  where
    indexNameOfNPK :: NPlusK -> Maybe IdentName
    indexNameOfNPK (NPlusK "" _) = Nothing
    indexNameOfNPK (NPlusK x _)  = Just x
indexNamesOfLhs  _            = PureVec Nothing


tupleContents :: (TupleF ∈ fs) => Lang fs -> [Lang fs]
tupleContents (Tuple xs) = concatMap tupleContents xs
tupleContents x          = [x]

matchToLhs :: (TupleF ∈ fs) => LExpr -> Lang fs -> GenM [(IdentName, Lang fs)]
matchToLhs l = matchToIdents (namesOfLhs l)

matchToIdents :: (TupleF ∈ fs) => TupleOfIdents -> Lang fs -> GenM [(IdentName, Lang fs)]
matchToIdents = go
  where
    go (Tuple xs) (Tuple ys) | length xs == length ys = do
                                 ms <- zipWithM go xs ys
                                 return $ concat ms
    go (Tuple _) (Tuple _) = raiseErr $ failed "tuple length mismatch."
    go (Tuple _) _         = raiseErr $ failed "the LHS expects a tuple, but RHS is not a tuple."
    go (Ident x) y = return [(x,y)]
    go _ _ = error "no match(Formura.OrthotopeMachine.Translate.matchToIdents)"


matchValueExprToLhs :: LExpr -> ValueExpr -> GenM [(IdentName, ValueExpr)]
matchValueExprToLhs (Grid npk lx) vx = do
  ivx <- matchToLhs lx vx
  forM ivx $ \(i,v) -> do
    v2 <- gen (GridF (negate npk) (return v))
    return (i,v2)
matchValueExprToLhs lx vx = matchToLhs lx vx

-- | Create a Binding so that names in the LExpr become free variables,
lexicalScopeHolder :: LExpr -> LexBinding
lexicalScopeHolder l =
  let xs :: [TupleOfIdents]
      xs = tupleContents $ namesOfLhs l
      f :: TupleOfIdents -> (IdentName, ValueLexExpr)
      f (Ident x) = (x, Ident x)
      f _         = error "unexpected happened in creating a scopeHolder."
  in M.fromList $ map f xs


matchTupleRtoL :: (TupleF ∈ fs, TupleF ∈ gs) => Lang fs -> Lang gs -> GenM [(Lang fs, Lang gs)]
matchTupleRtoL = go
  where
    go (Tuple xs) (Tuple ys) | length xs == length ys = do
                                 ms <- zipWithM go xs ys
                                 return $ concat ms
    go (Tuple _) (Tuple _) = raiseErr $ failed "tuple length mismatch."
    go (Tuple _) _         = raiseErr $ failed "the LHS expects a tuple, but RHS is not a tuple."
    go x y = return [(x,y)]

withBindings :: BindingF (GenM ValueExpr) -> GenM a -> GenM a
withBindings b1 genX = do
  let
      typeDecls0 :: [(LExpr, TypeExpr)]
      typeDecls0 = typeDecls b1

      substs0 :: [(LExpr, GenM ValueExpr)]
      substs0 = substs b1

  let evalTypeDecl :: (LExpr, TypeExpr) -> GenM [(IdentName, TypeExpr)]
      evalTypeDecl (l, t) = matchToLhs l t
  let evalTypeModDecl :: (LExpr, TypeModifier) -> [(IdentName, TypeModifier)]
      evalTypeModDecl (l, tm) = [(n, tm) | Ident n <- tupleContents $ namesOfLhs l]

  (typeDict :: M.Map IdentName TypeExpr)
    <- (M.fromList . concat) <$> mapM evalTypeDecl typeDecls0
  let typeModDict :: M.Map IdentName [TypeModifier]
      typeModDict = M.unionsWith (++) $
                    map (\(ident, tm) -> M.singleton ident [tm]) $
                    typeModifiers b1 >>= evalTypeModDecl

  -- select all external function declarations
  let extFuns :: [IdentName]
      extFuns = [ f
                | (Ident f, FunType) <- typeDecls0
                , Just tmde <- [M.lookup f typeModDict]
                , TMExtern `elem` tmde]

      extFunBinds :: Binding
      extFunBinds = M.fromList [ ( f, FunValue (Ident "q") (Uniop ("external-call/" ++ f) (Ident "q")))
                     | f <- extFuns]
  let
    -- make bindings enter scope one by one, not simultaneously
    graduallyBind :: [(LExpr, GenM ValueExpr)] -> GenM [(IdentName, ValueExpr)]
    graduallyBind [] = return []
    graduallyBind ((l0,genV): restOfBinds) = do
      let lis :: Vec (Maybe IdentName)
          lis = indexNamesOfLhs l0
          idBs :: [(IdentName, Int)]
          idBs = [(x, ax) | (Just x , ax) <- zip (toList lis) [0..]]

      indexBindings <- forM idBs $ \ (x, ax) -> do
        v <- insert (LoadIndex ax) (ElemType "Rational")
        return (x, v)

      v0 <- local (binding %~ M.union (M.fromList indexBindings)) genV
      lvs <- matchValueExprToLhs l0 v0
      nvs <- forM lvs $ \ (name0, v1) -> do
        v <- case M.lookup name0 typeDict of
          Nothing -> return v1
          Just t  -> castVal t v1

        let
          annV (n :. _) = do
             theGraph . ix n . A.annotation %= A.weakSet (SourceName name0)
             let isManifest = do
                   ms <- M.lookup name0 typeModDict
                   return $ TMManifest `elem` ms
             when (isManifest == Just True) $ do
               theGraph . ix n . A.annotation %= A.set Manifest
               theGraph . ix n . A.annotation %= A.set (SourceName name0)

          annV (Tuple xs) = mapM_ annV xs
          annV _ = return ()

        annV v

        return (name0, v)

      nvs2 <- local (binding %~ M.union (M.fromList nvs)) $ graduallyBind restOfBinds
      return $ nvs ++ nvs2

  local (binding %~ M.union extFunBinds) $ do
    substs1 <- graduallyBind substs0
    local (binding %~ M.union (M.fromList substs1)) genX




instance Generatable (Sum '[]) where
  gen _ =  raiseErr $ failed "impossible happened: gen of Void"

instance Generatable NodeValueF where
  gen (NodeValue t v) = return (NodeValue t v)
  gen _ = error "no match (Formura.OrthotopeMachine.Translate.gen)"

instance Generatable FunValueF where
  gen (FunValue l r) = return (FunValue l r)
  gen _ = error "no match (Formura.OrthotopeMachine.Translate.gen)"

instance (Generatable f, Generatable (Sum fs)) => Generatable (Sum (f ': fs)) where
  gen =  gen +:: gen

genRhs :: RXExpr -> GenM ValueExpr
genRhs = compilerFoldout gen

toNodeType :: TypeExpr -> GenM OMNodeType
toNodeType (ElemType x) = return $ ElemType x
toNodeType (GridType v x) = do
  x2 <- toNodeType x
  return $ GridType v x2
toNodeType t = raiseErr $ failed $ "incompatible type `" ++ show t ++ "` encountered in conversion to node type."

-- | Generate code for a global function. This generates 'Load' and 'Store' nodes,
--   in addition to all the usual computation nodes.

genGlobalFunction :: BindingF RExpr -> TypeExpr -> LExpr -> RExpr -> GenM TypeExpr
genGlobalFunction globalBinding inputType outputPattern (Lambda l r) =  bindThemAll $ do
  typedLhs <- matchToLhs l inputType
  initBinds <- forM typedLhs $ \(name1, t1) -> do

    t1d <- toNodeType t1
    v1 <- insert (Load name1) t1d

    return (name1, v1)

  returnValueExpr <- local (M.union $ M.fromList initBinds) $ genRhs $ subFix r

  rvElems <- matchToLhs outputPattern returnValueExpr

  forM_ rvElems $ \ (name1, rv1) ->
    case rv1 of
      (n99 :. _ ) -> do
        (n100 :. _) <- insert (Store name1 n99) unitType
        theGraph . ix n100 . A.annotation %= A.set Manifest
        theGraph . ix n100 . A.annotation %= A.weakSet (SourceName $ name1 ++ "_next")
      _           -> raiseErr $ failed "The return type of a global function must be a tuple of grids."

  return $ typeExprOf returnValueExpr
  where
    bindThemAll = withBindings $ fmap (genRhs .subFix) globalBinding
genGlobalFunction _ _ _ _ = raiseErr $ failed "Identifier specified for function generation is not of function type."

lookupToplevelIdents :: Program -> IdentName -> GenM RExpr
lookupToplevelIdents fprog name0 =  case lup stmts of
  [] -> raiseErr $ failed $ "Identifier `" ++ name0 ++ "` not found."
  [x] -> return x
  _  -> raiseErr $ failed $ "Multiple declaration of identifier `" ++ name0 ++ "` found."
  where
    (Program _ (BindingF stmts) _) = fprog

    lup :: [StatementF RExpr] -> [RExpr]
    lup [] = []
    lup (SubstF (Ident nam) rhs : xs) | nam == name0 = rhs : lup xs
    lup (_:xs) = lup xs

-- FIX ME
lookupToplevelIdents' :: Program -> IdentName -> GenM (Maybe RExpr)
lookupToplevelIdents' fprog name0 =  case lup stmts of
  [] -> return Nothing
  [x] -> return (Just x)
  _  -> raiseErr $ failed $ "Multiple declaration of identifier `" ++ name0 ++ "` found."
  where
    (Program _ (BindingF stmts) _) = fprog

    lup :: [StatementF RExpr] -> [RExpr]
    lup [] = []
    lup (SubstF (Ident nam) rhs : xs) | nam == name0 = rhs : lup xs
    lup (_:xs) = lup xs

calcSleeve :: OMGraph -> Int
calcSleeve g = maximum $ map traceNode $ findStore g
  where
    findStore :: OMGraph -> [OMNodeID]
    findStore = M.foldr getStore []

    getStore (Node (Store _ i) _ _) acc = i:acc
    getStore _ acc = acc

    traceNode :: OMNodeID -> Int
    traceNode i0 = maximum $ go i0 (pure 0)
      where
        go :: OMNodeID -> Vec Int -> Vec Int
        go i s =
          case M.lookup i g of
            Just (Node (Load _) _ _) -> s
            Just (Node (LoadIndex _) _ _) -> s
            Just (Node (LoadExtent _) _ _) -> s
            Just (Node (Imm _) _ _) -> s
            Just (Node (Uniop _ i') _ _) -> go i' s
            Just (Node (Binop _ i1 i2) _ _) -> go i1 s `max` go i2 s
            Just (Node (Triop _ i1 i2 i3) _ _) -> maximum [go i1 s,go i2 s,go i3 s]
            Just (Node (Naryop _ is) _ _) -> maximum [go i' s | i' <- is]
            Just (Node (Shift d i') _ _) -> go i' (s + abs d)
            _ -> s

genOMProgram :: Program -> IO OMProgram
genOMProgram fprog = do
  let run g = runCompilerRight g defaultCodegenRead defaultCodegenState
      gbinds = fprog ^. programBinding
  (lhsOfStep,_,_) <- run $ do
    (Lambda l _) <- lookupToplevelIdents fprog "step"
    return l

  (initType, stInit, _) <- run $ do
    setupGlobalEnvironment fprog
    initFunDef <- lookupToplevelIdents fprog "init"
    genGlobalFunction gbinds (Tuple []) lhsOfStep initFunDef

  (existFirstStep , stFirst, _) <- run $ do
    setupGlobalEnvironment fprog
    mfirstStepFunDef <- lookupToplevelIdents' fprog "first_step"
    case mfirstStepFunDef of
      Nothing -> return False
      Just firstStepFunDef -> do
        firstStepType <- genGlobalFunction gbinds initType lhsOfStep firstStepFunDef
        when (initType /= firstStepType) $
          raiseErr $ failed $ "the return type of first_step : " ++ show firstStepType ++ "\n" ++
            "must match the return type of init : " ++ show initType
        return True

  (existFilter , stFilter, _) <- run $ do
    setupGlobalEnvironment fprog
    mfilterFunDef <- lookupToplevelIdents' fprog "filter"
    case mfilterFunDef of
      Nothing -> return False
      Just filterFunDef -> do
        filterType <- genGlobalFunction gbinds initType lhsOfStep filterFunDef
        when (initType /= filterType) $
          raiseErr $ failed $ "the return type of filter : " ++ show filterType ++ "\n" ++
            "must match the return type of init : " ++ show initType
        return True

  (stateSignature0, stStep, _) <- run $ do
    stepFunDef <- lookupToplevelIdents fprog "step"
    setupGlobalEnvironment fprog
    stepType <- genGlobalFunction gbinds initType lhsOfStep stepFunDef
    when (initType /= stepType) $
      raiseErr $ failed $ "the return type of step : " ++ show stepType ++ "\n" ++
        "must match the return type of init : " ++ show initType

    bs99 <- matchToLhs lhsOfStep stepType
    return $ M.fromList bs99

  let sleeve = calcSleeve (stStep ^. theGraph)
      sleeve0 = if existFirstStep then Just (calcSleeve $ stFirst ^. theGraph) else Nothing
      sleeveFilter = if existFilter then Just (calcSleeve $ stFilter ^. theGraph) else Nothing
  case convertConfig sleeve sleeve0 sleeveFilter (fprog ^. programNumericalConfig) of
    Left err -> die $ "Error: " ++ displayException err
    Right cfg -> do
      return MachineProgram
        { _omGlobalEnvironment = (stInit ^. globalEnvironment) & envNumericalConfig .~ cfg
        , _omInitGraph = stInit ^. theGraph
        , _omFirstStepGraph = if existFirstStep then Just (stFirst ^. theGraph) else Nothing
        , _omFilterGraph = if existFilter then Just (stFilter ^. theGraph) else Nothing
        , _omStepGraph = stStep ^. theGraph
        , _omStateSignature = stateSignature0
        }
