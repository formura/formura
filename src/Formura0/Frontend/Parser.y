{
{-# LANGUAGE OverloadedStrings #-}
module Formura0.Frontend.Parser where

import Formura0.Syntax
import Formura0.Frontend.Lexer
import Formura0.Frontend.ParserMonad
}

%name happyParser
%tokentype { TokenWithPos }

%monad { Parser } { thenP } { returnP }
%lexer { lexer } { TokenWith _ TokenEOF }

%token
  let      { TokenWith _ TokenLet }
  in       { TokenWith _ TokenIn  }
  end      { TokenWith _ TokenEnd }
  fun      { TokenWith _ TokenFun }
  imm      { TokenWith _ (TokenImm $$) }
  var      { TokenWith _ (TokenVar $$) }
  '+'      { TokenWith _ TokenAdd }
  '-'      { TokenWith _ TokenSub }
  '*'      { TokenWith _ TokenMul }
  '/'      { TokenWith _ TokenDiv }
  '='      { TokenWith _ TokenEq }
  '('      { TokenWith _ TokenOP }
  ')'      { TokenWith _ TokenCP }
  ','      { TokenWith _ TokenSep }
  const    { TokenWith _ TokenConst }
  extern   { TokenWith _ TokenExtern }
  manifest { TokenWith _ TokenManifest }

%right in
%left '+' '-'
%left '*' '/'

%%

program : decl                        { $1 }
        | program decl                { $1 <> $2 }

decl : texp lexp '=' rexp             { [TypeDecl (getPos $3) $1 $2, Subst (getPos $3) $2 $4] }
     | lexp '=' rexp                  { [Subst (getPos $2) $1 $3] }

texp : typeMod texp0                  { ModifiedTypeExpr $1 $2 }

typeMod :                             { [] }
        | const typeMod               { TMConst:$2 }
        | extern typeMod              { TMExtern:$2 }
        | manifest typeMod            { TMManifest:$2 }

texp0 : var                           { ElemType $1 }
      | '(' tupleOftype ')'           { TupleType $2 }

tupleOftype : texp0                   { [$1] }
            | texp0 ',' tupleOftype   { $1:$3 }

lexp : var                            { IdentL $1 }
     | '(' tupleOflexp ')'            { TupleL $2 }

tupleOflexp : lexp                   { [$1] }
            | lexp ',' tupleOflexp   { $1:$3 }

rexp : rexp '+' rexp                  { Binop "+" $1 $3 }
     | rexp '-' rexp                  { Binop "-" $1 $3 }
     | rexp '*' rexp                  { Binop "*" $1 $3 }
     | rexp '/' rexp                  { Binop "/" $1 $3 }
     | term                           { $1 }

term : '(' rexp ')'                   { $2 }
     | '(' tupleOfrexp ')'            { TupleR $2 }
     | var                            { IdentR $1 }
     | imm                            { Imm (toRational $1) }

tupleOfrexp : rexp                   { [$1] }
            | rexp ',' tupleOfrexp   { $1:$3 }
