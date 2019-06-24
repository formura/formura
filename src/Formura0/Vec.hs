{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Formura0.Vec where

import Control.Applicative

newtype Vec a = Vec (ZipList a)
  deriving (Eq,Show,Functor,Applicative)
