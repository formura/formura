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
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import Formura.OrthotopeMachine.Graph
import Formura.Vec

data BlockingType = NoBlocking

data CType = CVoid
           | CInt
           | CFloat
           | CDouble
           | CArray (Vec Int) CType
           | CStruct String [(String, CType)]
           | CPtr CType
           | CRawType String

data CTypedef = CTypedef CType String
              | CTypedefStruct [(String, CType)] String

data Kind = Normal
          | AoS (Vec Int)
          | SoA (Vec Int)

data CVariable = CVariable
  { variableName :: String
  , variableType :: CType
  , variableValue :: Maybe String
  , variableLifetime :: Maybe String
  }

data CStatement = Copy
                | Sendrecv
                | Call
                | Raw

data CFunction = CFunction
  { functionName :: String
  , returnedType :: CType
  , arguments :: [(CType,String)]
  , functionBody :: [CStatement]
  }

data CodeStructure = CodeStructure
  { _headers :: [String]
  , _globalVariables :: [CVariable]
  , _globalTypes :: [CTypedef]
  , _localTypes :: [CTypedef]
  , _globalFunctions :: [CFunction]
  , _localFunctions :: [CFunction]
  , _hFileName :: String
  , _cFileName :: String
  }

makeLenses ''CodeStructure

newtype GenM a = GenM { unwrapGenM :: ReaderT MMProgram (State CodeStructure) a }
  deriving (Functor, Applicative, Monad, MonadReader MMProgram, MonadState CodeStructure)

generate :: MMProgram -> String -> String -> GenM () -> CodeStructure
generate mm hfn cfn = (flip execState) cs0 . (flip runReaderT) mm . unwrapGenM
  where cs0 = CodeStructure [] [] [] [] [] [] hfn cfn

newtype BuildM m a = BuildM { unwrapBuildM :: WriterT [CStatement] m a }
  deriving (Functor, Applicative, Monad)

deriving instance MonadReader MMProgram m => MonadReader MMProgram (BuildM m)
deriving instance MonadState CodeStructure m => MonadState CodeStructure (BuildM m)
deriving instance Monad m => MonadWriter [CStatement] (BuildM m)

build :: IsGen m => BuildM m a -> m (a, [CStatement])
build = runWriterT . unwrapBuildM

type IsGen m = (MonadReader MMProgram m, MonadState CodeStructure m)

