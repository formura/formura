{-# LANGUAGE TemplateHaskell #-}

module Formura.GlobalEnvironment where

import Control.Lens
import Data.Char (toUpper)

import Formura.NumericalConfig
import Formura.Syntax
import Formura.Vec

data GlobalEnvironment = GlobalEnvironment
  { _dimension :: Int
  , _axesNames :: Vec IdentName
  , _envNumericalConfig :: InternalConfig
  } deriving (Eq, Ord, Show)

makeClassy ''GlobalEnvironment

defaultGlobalEnvironment :: GlobalEnvironment
defaultGlobalEnvironment = GlobalEnvironment 0 (Vec []) defaultInternalConfig
