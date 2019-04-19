{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Formura0.Syntax where

import Data.Text (Text)

import Formura.Vec
import Formura0.Frontend.Lexer
-- ^ 再実装する必要がある

-- * 構文木

type IdentName = Text

data NPlusK = NPlusK IdentName Rational
  deriving (Eq,  Show)

data LExpr = GridL !(Vec NPlusK) LExpr
           | TupleL [LExpr]
           | Vector !IdentName LExpr -- deprecated
           | IdentL !IdentName
  deriving (Eq,  Show)

data RExpr = Let !Binding RExpr
           | Lambda LExpr RExpr
           | Apply RExpr RExpr
           | GridR  !(Vec NPlusK) RExpr
           | TupleR [RExpr]
           | Uniop  !IdentName RExpr
           | Binop !IdentName RExpr RExpr
           | Triop !IdentName RExpr RExpr RExpr
           | Naryop !IdentName [RExpr]  -- deprecated
           | IdentR !IdentName
           | Imm !Rational
  deriving (Eq, Show)

data TypeExpr = TopType
              | GridType !(Vec Rational) TypeExpr
              | TupleType [TypeExpr]
              | VectorType !Int TypeExpr -- deprecated
              | FunType
              | ElemType !IdentName
  deriving (Eq, Show)

data TypeModifier = TMConst
                  | TMManifest
                  | TMExtern
  deriving (Eq, Ord, Show)

data ModifiedTypeExpr = ModifiedTypeExpr ![TypeModifier] !TypeExpr
  deriving (Eq, Show)

data Statement = Subst !AlexPosn !LExpr !RExpr
               -- ^ substitution
               | TypeDecl !AlexPosn !ModifiedTypeExpr !LExpr
               -- ^ type declaration with modification
               | SpecialDecl !AlexPosn !SpecialDeclaration
  deriving (Eq, Show)

type Binding = [Statement]

data SpecialDeclaration = DimensionDeclaration !Int
                        | AxesDeclaration ![IdentName]
                        | GridStructTypeNameDeclaration !IdentName
                        | GridStructInstanceNameDeclaration !IdentName
  deriving (Eq, Show)

newtype Program = Program [Statement]
  deriving (Eq, Show, Semigroup, Monoid)

