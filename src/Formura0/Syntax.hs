module Formura0.Syntax where

import Data.Text (Text)

import Formura0.Frontend.Lexer
import Formura0.Vec

type IdentName = Text

data Decl t l r = TypeDecl !AlexPosn !(ModifiedType t) !l
                | VarDecl !AlexPosn !l !r
                | SpcDecl !AlexPosn !SpecialDeclaration
  deriving (Eq,Show)

type Program = [Statement]

-- | type for parser
type Statement0 = Decl Exp Exp Exp'

-- | type for AST
type Statement = Decl TExp LExp RExp

data Exp = Ident !IdentName
         | Tuple [Exp]
         | Grid ![NPlusK] Exp
         | None
  deriving (Eq,Show)

data Exp' = Ident' !IdentName
          | Imm' !Rational
          | Tuple' [Exp']
          | Grid' ![NPlusK] Exp'
          | Uniop' !Op1 Exp'
          | Binop' !Op2 Exp' Exp'
          | Let' ![Statement0] Exp'
          | Lambda' ![Exp] Exp'
          | If' Exp' Exp' Exp'
          | App' Exp' Exp'
  deriving (Eq,Show)

data TExp = IdentT !IdentName
          | TupleT [TExp]
          | GridT !(Vec Rational) TExp
          | SomeType
  deriving (Eq,Show)

data ModifiedType a = ModifiedType ![TypeModifier] a
  deriving (Eq,Show)

data TypeModifier = TMConst
                  | TMManifest
                  | TMExtern
  deriving (Eq,Show)

data LExp = IdentL !IdentName
          | TupleL [LExp]
          | GridL !(Vec IdentName) LExp
  deriving (Eq,Show)

data RExp = IdentR !IdentName
          | ImmR !Rational
          | TupleR [RExp]
          | GridR !(Vec NPlusK) RExp
          | UniopR !Op1 RExp
          | BinopR !Op2 RExp RExp
          | LetR ![Statement] RExp
          | LambdaR ![LExp] RExp
          | IfR RExp RExp RExp
          | AppR RExp RExp
  deriving (Eq,Show)

data Op1 = Plus
         | Minus
  deriving (Eq,Show)

data Op2 = Add
         | Sub
         | Mul
         | Div
         | Pow
         | And
         | Or
         | Eq
         | NEq
         | Lt
         | LEq
         | Gt
         | GEq
  deriving (Eq,Show)

data SpecialDeclaration = Dimension !Int
                        | Axes ![IdentName]
                        | GSTypeName !IdentName
                        | GSInstanceName !IdentName
  deriving (Eq,Show)

data NPlusK = NPlusK !IdentName !Rational
  deriving (Eq,Show)
