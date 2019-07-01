module Formura0.OMGraph where

import qualified Data.Map as M

import Formura.NumericalConfig
import Formura0.Annotation
import Formura0.Syntax
import Formura0.Vec

type OMID = Int

data OMInst = Load !(Vec Int) !OMID
            | Store !IdentName !OMID
            | LoadGlobal !(Vec Int) !IdentName
            | LoadIndex !Int
            | Uniop !Op1 !OMID
            | Binop !Op2 !OMID !OMID
            | If !OMID !OMID !OMID
            | Imm !Rational
            | Call1 !IdentName ![OMID]
            -- | CallN !IdentName ![OMID] ![OMID]
  deriving (Show)

data OMNode = OMNode
  { inst    :: !OMInst
  , theType :: !TExp
  , annot   :: ![Annot]
  } deriving (Show)

type OMGraph = M.Map OMID OMNode

type GlobalVariables = [(IdentName, TExp)]

data OMProgram = OMProgram
  { config                 :: InternalConfig
  , dimension              :: Int
  , axesNames              :: [IdentName]
  , gridStructTypeName     :: IdentName
  , gridStructInstanceName :: IdentName
  , commBases              :: [[Int]]
  , globalVariables        :: GlobalVariables
  , initGraph              :: OMGraph
  , firstStepGraph         :: Maybe OMGraph
  , filterGraph            :: Maybe OMGraph
  , stepGraph              :: OMGraph
  } deriving (Show)

-- OMGraph に関する操作
--  - AST から Graph を構築する
--  - Graph 上でカーネルを分割する (サブグラフを作る)
--  - Graph 上のシフト操作を持ち上げる
--  - 共通部分式の削除
