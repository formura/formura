{
{-# LANGUAGE OverloadedStrings #-}
module Formura0.Frontend.Parser where

import Formura0.Syntax
import Formura0.Frontend.Lexer
import Formura0.Frontend.ParserMonad
import Formura.Vec
}

%name happyParser
%tokentype { TokenWithPos }

%monad { Parser } { thenP } { returnP }
%lexer { lexer } { TokenWith _ TokenEOF }

%token
  let                       { TokenWith _ TokenLet }
  in                        { TokenWith _ TokenIn  }
  end                       { TokenWith _ TokenEnd }
  fun                       { TokenWith _ TokenFun }
  imm                       { TokenWith _ (TokenImm $$) }
  int                       { TokenWith _ (TokenInt $$) }
  var                       { TokenWith _ (TokenVar $$) }
  '+'                       { TokenWith _ TokenAdd }
  '-'                       { TokenWith _ TokenSub }
  '*'                       { TokenWith _ TokenMul }
  '/'                       { TokenWith _ TokenDiv }
  '='                       { TokenWith _ TokenEq }
  '('                       { TokenWith _ TokenOP }
  ')'                       { TokenWith _ TokenCP }
  '['                       { TokenWith _ TokenOB }
  ']'                       { TokenWith _ TokenCB }
  ','                       { TokenWith _ TokenSep }
  const                     { TokenWith _ TokenConst }
  extern                    { TokenWith _ TokenExtern }
  manifest                  { TokenWith _ TokenManifest }
  "::"                      { TokenWith _ TokenTypeSep }
  dimension                 { TokenWith _ TokenDim }
  axes                      { TokenWith _ TokenAxes }
  grid_struct_type_name     { TokenWith _ TokenGridStructTypeName }
  grid_struct_instance_name { TokenWith _ TokenGridStructInstanceName }
  function                  { TokenWith _ TokenFunction }


%right in
%left '+' '-'
%left '*' '/'

%%

program : decl                                   { Program $1 }
        | program decl                           { $1 <> Program $2 }

decl : decl0                                     { $1 }
     | specialDecl                               { $1 }

specialDecl : dimension "::" int                 { [SpecialDecl (getPos $1) $ DimensionDeclaration $3] }
            | axes "::" axesLabels               { [SpecialDecl (getPos $1) $ AxesDeclaration $3] }
            | grid_struct_type_name "::" var     { [SpecialDecl (getPos $1) $ GridStructTypeNameDeclaration $3] }
            | grid_struct_instance_name "::" var { [SpecialDecl (getPos $1) $ GridStructInstanceNameDeclaration $3] }

axesLabels : var                                 { [$1] }
           | axesLabels ',' var                  { $1 <> [$3] }

-- TODO: 1行に複数の宣言を許容するようにする
-- TODO: 右辺式がないものもサポートする
decl0 : texp "::" lexp '=' rexp                  { [TypeDecl (getPos $4) $1 $3, Subst (getPos $4) $3 $5] }
      | texp0 "::" lexp '=' rexp                 { [TypeDecl (getPos $4) (ModifiedTypeExpr [] $1) $3, Subst (getPos $4) $3 $5] }
      | lexp '=' rexp                            { [Subst (getPos $2) $1 $3] }

texp : typeMod texp0                             { ModifiedTypeExpr $1 $2 }

typeMod :                                        { [] }
        | const typeMod                          { TMConst:$2 }
        | extern typeMod                         { TMExtern:$2 }
        | manifest typeMod                       { TMManifest:$2 }

texp0 : function                                 { FunType }
      | var '[' indexOfgridtype ']'              { GridType (Vec $3) (ElemType $1) }
      | var                                      { ElemType $1 }
      | '(' tupleOftype ')'                      { TupleType $2 }

tupleOftype : texp0                              { [$1] }
            | texp0 ',' tupleOftype              { $1:$3 }

indexOfgridtype : ratOrEmpty                     { $1 }
                | ratOrEmpty ',' indexOfgridtype { $1 <> $3 }

ratOrEmpty :                                     { [] }
           | rat                                 { [$1] }

rat : rat '+' rat                                { $1 + $3 }
    | rat '-' rat                                { $1 - $3 }
    | rat '*' rat                                { $1 * $3 }
    | rat '/' rat                                { $1 / $3 }
    | numExp                                     { $1 }

numExp : '(' rat ')'                             { $2 }
       | int                                     { toRational $1 }
       | imm                                     { toRational $1 }

lexp : var                                       { IdentL $1 }
     | var '[' nPlusK ']'                        { GridL (Vec $3) (IdentL $1) }
     | '(' tupleOflexp ')'                       { TupleL $2 }

nPlusK : nPlusK0                                 { $1 }
       | nPlusK0 ',' nPlusK                      { $1 <> $3 }

nPlusK0 :                                        { [] }
        | var                                    { [NPlusK $1 0] }
        | rat                                    { [NPlusK "" $1] }
        | var '+' rat                            { [NPlusK $1 $3] }
        | var '-' rat                            { [NPlusK $1 (negate $3)] }

tupleOflexp : lexp                               { [$1] }
            | lexp ',' tupleOflexp               { $1:$3 }

rexp : rexp '+' rexp                             { Binop "+" $1 $3 }
     | rexp '-' rexp                             { Binop "-" $1 $3 }
     | rexp '*' rexp                             { Binop "*" $1 $3 }
     | rexp '/' rexp                             { Binop "/" $1 $3 }
     | term                                      { $1 }

term : '(' rexp ')'                              { $2 }
     | '(' tupleOfrexp ')'                       { TupleR $2 }
     | var                                       { IdentR $1 }
     | imm                                       { Imm (toRational $1) }

tupleOfrexp : rexp                               { [$1] }
            | rexp ',' tupleOfrexp               { $1:$3 }
