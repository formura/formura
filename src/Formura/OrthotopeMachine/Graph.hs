{- |
Copyright   : (c) Takayuki Muranushi, 2015
License     : MIT
Maintainer  : muranushi@gmail.com
Stability   : experimental

A virtual machine with multidimensional vector instructions that operates on structured lattices, as described
in http://arxiv.org/abs/1204.4779 .
-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Formura.OrthotopeMachine.Graph where

import           Algebra.Lattice
import           Control.Lens
import           Data.Data
import qualified Data.Map as M
import           Text.Read (Read(..))

import qualified Formura.Annotation as A
import           Formura.GlobalEnvironment
import           Formura.Language.Combinator
import           Formura.Syntax
import           Formura.Type
import           Formura.Vec

-- | The functor for orthotope machine-specific instructions. Note that arithmetic operations are outsourced.

data LoadUncursoredF x = LoadF IdentName
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data DataflowInstF x = StoreF IdentName x
                     | LoadIndexF Int
                     | LoadExtentF Int
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | The functor for language that support shift operations.
data ShiftF x = ShiftF (Vec Int) x
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | The functor for language that support cursored load of graph nodes.
data LoadCursorF x = LoadCursorF (Vec Int) OMNodeID
                   | LoadCursorStaticF (Vec Int) IdentName
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)


-- | smart patterns
pattern Load :: Matches LoadUncursoredF s => IdentName -> s
pattern Load n <- ((^? match) -> Just (LoadF n))
  where Load n = match # LoadF n

pattern Store :: Matches DataflowInstF s => IdentName -> Content DataflowInstF s -> s
pattern Store n x <- ((^? match) -> Just (StoreF n x))
  where Store n x = match # StoreF n x

pattern LoadIndex :: Matches DataflowInstF s => Int -> s
pattern LoadIndex n <- ((^? match) -> Just (LoadIndexF n))
  where LoadIndex n = match # LoadIndexF n

pattern LoadExtent :: Matches DataflowInstF s => Int -> s
pattern LoadExtent n <- ((^? match) -> Just (LoadExtentF n))
  where LoadExtent n = match # LoadExtentF n

pattern Shift :: Matches ShiftF s => Vec Int -> Content ShiftF s -> s
pattern Shift v x <- ((^? match) -> Just (ShiftF v x))
  where Shift v x = match # ShiftF v x

pattern LoadCursor :: Matches LoadCursorF s => Vec Int -> OMNodeID -> s
pattern LoadCursor v x <- ((^? match) -> Just (LoadCursorF v x))
  where LoadCursor v x = match # LoadCursorF v x

pattern LoadCursorStatic :: Matches LoadCursorF s => Vec Int -> IdentName -> s
pattern LoadCursorStatic v x <- ((^? match) -> Just (LoadCursorStaticF v x))
  where LoadCursorStatic v x = match # LoadCursorStaticF v x


newtype OMNodeID = OMNodeID Int
  deriving (Eq, Ord, Num, Data)

instance Show OMNodeID where
  showsPrec n (OMNodeID x) = showsPrec n x

instance Read OMNodeID where
  readPrec = fmap OMNodeID  readPrec

newtype MMNodeID = MMNodeID Int
  deriving (Eq, Ord, Num, Data)

instance Show MMNodeID where
  showsPrec n (MMNodeID x) = showsPrec n x

instance Read MMNodeID where
  readPrec = fmap MMNodeID  readPrec

-- | The instruction type for Orthotope Machine.
type OMInstF = Sum '[DataflowInstF, LoadUncursoredF, ShiftF, OperatorF, ImmF]
type OMInstruction = OMInstF OMNodeID

-- | The instruction type for Manifest Machine, where every node is manifest,
--   and each instruction is actually a subgraph for delayed computation
type MMInstF = Sum '[DataflowInstF, LoadCursorF, OperatorF, ImmF]
type MMInstruction = M.Map MMNodeID (Node MicroInstruction MicroNodeType)
type MicroInstruction = MMInstF MMNodeID

data MMLocation = MMLocation { _mmlOMNodeID :: OMNodeID,  _mmlCursor :: Vec Int}
  deriving(Eq, Ord, Show)


mmInstTails :: MMInstruction -> [MMInstF MMNodeID]
mmInstTails mminst = rets
  where
    rets = [_nodeInst nd
           | nd <- M.elems mminst
           , let Just (MMLocation omnid2 _) = A.viewMaybe nd
           , omnid2==omnid ]

    Just (MMLocation omnid _) = A.viewMaybe maxNode

    maxNode :: MicroNode
    maxNode = snd $ M.findMax mminst


type OMNodeType  = Fix OMNodeTypeF
type OMNodeTypeF = Sum '[ TopTypeF, GridTypeF, ElemTypeF ]

type MicroNodeType  = Fix MicroNodeTypeF
type MicroNodeTypeF = Sum '[ ElemTypeF ]


instance MeetSemiLattice OMNodeType where
  (/\) = semiLatticeOfOMNodeType

semiLatticeOfOMNodeType :: OMNodeType -> OMNodeType -> OMNodeType
semiLatticeOfOMNodeType a b = case go a b of
  TopType -> case go b a of
    TopType -> TopType
    c -> c
  c       -> c
  where
    go :: OMNodeType -> OMNodeType -> OMNodeType
    go x y | x == y = x
    go (ElemType ea) (ElemType eb) = subFix (ElemType ea /\ ElemType eb :: ElementalType)
    go x@(ElemType _) (GridType v c) = let d = x /\ c
                                        in if d==TopType then TopType else GridType v d
    go (GridType v1 c1) (GridType v2 c2) = if v1 == v2 then GridType v1 (c1 /\ c2)
                                                       else TopType
    go _ _ = TopType

mapElemType :: (IdentName -> IdentName) -> OMNodeType -> OMNodeType
mapElemType f (ElemType t) = ElemType $ f t
mapElemType f (GridType v t) = GridType v $ mapElemType f t
mapElemType _ TopType = TopType
mapElemType _ _ = error "no match(Formura.OrthotopeMachine.Graph.mapElemType"

data Node instType typeType = Node
  { _nodeInst :: instType
  , _nodeType :: typeType
  , _nodeAnnot :: A.Annotation
  }

instance (Eq i, Eq t) => Eq (Node i t) where
  (Node a b _) == (Node c d _) = (a,b) == (c,d)

instance (Ord i, Ord t) => Ord (Node i t) where
  compare (Node a b _) (Node c d _) = compare (a,b) (c,d)

instance (Show v, Show t) => Show (Node v t) where
  show (Node v t _) = show v ++ " :: " ++ show t

type OMNode = Node OMInstruction OMNodeType
type MMNode = Node MMInstruction OMNodeType
type MicroNode = Node MicroInstruction MicroNodeType

makeLenses ''Node

instance A.Annotated (Node v t) where
  annotation = nodeAnnot

-- instance (Data instType) => Data (Node instType typeType) where
--   gfoldl (*) z x = x{_nodeInst = gfoldl (*) z (_nodeInst x)}


type Graph instType typeType = M.Map OMNodeID (Node instType typeType)
type OMGraph = Graph OMInstruction OMNodeType
type MMGraph = Graph MMInstruction OMNodeType

data NodeValueF x = NodeValueF OMNodeID OMNodeType
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

pattern NodeValue :: Matches NodeValueF s => OMNodeID -> OMNodeType -> s
pattern NodeValue n t <- ((^? match) -> Just (NodeValueF n t))
  where NodeValue n t = match # NodeValueF n t

pattern (:.) :: Matches NodeValueF s => OMNodeID -> OMNodeType -> s
pattern n :. t <- ((^? match) -> Just (NodeValueF n t))
  where n :. t = match # NodeValueF n t


data FunValueF x = FunValueF LExpr RXExpr
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

pattern FunValue :: Matches FunValueF s => LExpr -> RXExpr -> s
pattern FunValue l r <- ((^? match) -> Just (FunValueF l r))
  where FunValue l r = match # FunValueF l r


-- | RXExpr is RExpr extended with NodeValue constructors
type RXExpr  = Fix RXExprF
type RXExprF = Sum '[ LetF, LambdaF, ApplyF, GridF, TupleF, OperatorF, IdentF, FunValueF, NodeValueF, ImmF ]
-- | 'ValueExpr' represents final forms of orthotope machine evaluation.
type ValueExpr = Fix ValueExprF
type ValueExprF = Sum '[TupleF, FunValueF, NodeValueF, ImmF]
-- | 'ValueLexExpr' extends 'ValueExpr' with unresolved identifiers. Expressions with free variables evaluate to 'ValueLexExpr' , not 'ValueExpr' .
type ValueLexExpr = Fix ValueLexExprF
type ValueLexExprF = Sum '[TupleF, FunValueF, NodeValueF, IdentF, ImmF]

instance Typed ValueExpr where
  typeExprOf (Imm _) = ElemType "Rational"
  typeExprOf (NodeValue _ t) = subFix t
  typeExprOf (FunValue _ _) = FunType
  typeExprOf (Tuple xs) = Tuple $ map typeExprOf xs
  typeExprOf _ = error "no match (Formura.OrthotopeMachine.Graph.typeExprOf"

data MachineProgram instType typeType = MachineProgram
  { _omGlobalEnvironment :: GlobalEnvironment
  , _omInitGraph :: Graph instType typeType
  , _omFirstStepGraph :: Maybe (Graph instType typeType)
  , _omFilterGraph :: Maybe (Graph instType typeType)
  , _omStepGraph :: Graph instType typeType
  , _omStateSignature :: M.Map IdentName TypeExpr
  }

makeClassy ''MachineProgram


type OMProgram = MachineProgram OMInstruction OMNodeType
type MMProgram = MachineProgram MMInstruction OMNodeType

instance HasGlobalEnvironment (MachineProgram v t) where
  globalEnvironment = omGlobalEnvironment


makeClassy ''MMLocation
