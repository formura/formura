{-# LANGUAGE TemplateHaskell #-}

module Formura.GlobalEnvironment where

import Control.Lens

import Formura.NumericalConfig
import Formura.Syntax

data GlobalEnvironment = GlobalEnvironment
  { _dimension :: Int
  , _axesNames :: [IdentName]
  , _envNumericalConfig :: InternalConfig
  } deriving (Eq, Ord, Show)

makeClassy ''GlobalEnvironment

defaultGlobalEnvironment :: GlobalEnvironment
defaultGlobalEnvironment = GlobalEnvironment 0 [] defaultInternalConfig
