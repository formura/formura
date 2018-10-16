{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Formura.Generator.Types where

import Control.Lens
import Control.Monad.RWS
import qualified Data.HashMap.Lazy as HM

import Formura.OrthotopeMachine.Graph

data CType = CVoid
           | CInt
           | CFloat
           | CDouble
           | CArray [Int] CType
           | CStruct String [(String, CType)]
           | CPtr CType
           | CRawType String

data CTypedef = CTypedef CType String
              | CTypedefStruct [(String, CType)] String

data Kind = Normal
          | AoS [Int]
          | SoA [Int]

data CVariable = CVariable
  { variableName :: String
  , variableType :: CType
  , variableValue :: Maybe String
  , variableLifetime :: Maybe String
  }

data CStatement = Decl CVariable
                | Bind String String
                | Loop [(String,Int,Int,Int)] [CStatement]
                | Call String [String]
                | Raw String

data CFunction = CFunction
  { functionName :: String
  , returnedType :: CType
  , arguments :: [(CType,String)]
  , functionBody :: [CStatement]
  }

data CodeStructure = CodeStructure
  { _headers :: [String]
  , _definedParams :: [String]
  , _globalVariables :: [CVariable]
  , _localVariables :: [CVariable]
  , _globalTypes :: [CTypedef]
  , _localTypes :: [CTypedef]
  , _globalFunctions :: [CFunction]
  , _localFunctions :: [CFunction]
  }

makeLenses ''CodeStructure

instance Semigroup CodeStructure where
  (CodeStructure h1 p1 gv1 lv1 gt1 lt1 gf1 lf1) <> (CodeStructure h2 p2 gv2 lv2 gt2 lt2 gf2 lf2) =
    CodeStructure (h1<>h2) (p1<>p2) (gv1<>gv2) (lv1<>lv2) (gt1<>gt2) (lt1<>lt2) (gf1<>gf2) (lf1<>lf2)

instance Monoid CodeStructure where
  mempty = CodeStructure [] [] [] [] [] [] [] []

data Table = Table
  { _stack :: [CStatement]
  , _variables :: HM.HashMap String CVariable
  , _target :: [(String,CType)]
  }

makeLenses ''Table

type BuildM = RWS MMProgram CodeStructure Table

genCodeStructure :: MMProgram -> BuildM () -> CodeStructure
genCodeStructure mm f = snd $ evalRWS f mm t0
  where t0 = Table [] HM.empty []

