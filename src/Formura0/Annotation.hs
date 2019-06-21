module Formura0.Annotation where

import Formura0.Syntax

data Annot = SourceName !IdentName
           | ManifestNode
  deriving (Eq,Show)
