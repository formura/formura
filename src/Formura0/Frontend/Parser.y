{
{-# LANGUAGE OverloadedStrings #-}
module Formura0.Frontend.Parser where

import Formura0.Frontend.ParserMonad
import Formura0.Frontend.Lexer
import Formura0.Syntax
}


%name happyParser
%tokentype { TokenWithPos }
%monad { Parser } { thenP } { returnP }
%lexer { lexer } { TokenWith _ TokenEOF }


%token
  ';'                       { TokenWith _ TokenEOS }
  '='                       { TokenWith _ TokenEq }
  '.'                       { TokenWith _ TokenCom }
  '+'                       { TokenWith _ TokenAdd }
  '-'                       { TokenWith _ TokenSub }
  '*'                       { TokenWith _ TokenMul }
  '/'                       { TokenWith _ TokenDiv }
  "**"                      { TokenWith _ TokenPow }
  '('                       { TokenWith _ TokenOP }
  ')'                       { TokenWith _ TokenCP }
  '['                       { TokenWith _ TokenOB }
  ']'                       { TokenWith _ TokenCB }
  ','                       { TokenWith _ TokenSep }
  "::"                      { TokenWith _ TokenTypeSep }
  "&&"                      { TokenWith _ TokenAnd }
  "||"                      { TokenWith _ TokenOr }
  "=="                      { TokenWith _ TokenEQ }
  "!="                      { TokenWith _ TokenNEQ }
  '<'                       { TokenWith _ TokenLT }
  "<="                      { TokenWith _ TokenLE }
  '>'                       { TokenWith _ TokenGT }
  ">="                      { TokenWith _ TokenGE }
  int                       { TokenWith _ (TokenInt $$) }
  float                     { TokenWith _ (TokenFloat $$) }
  var                       { TokenWith _ (TokenVar $$) }
  dimension                 { TokenWith _ TokenDim }
  axes                      { TokenWith _ TokenAxes }
  grid_struct_type_name     { TokenWith _ TokenGSTypeName }
  grid_struct_instance_name { TokenWith _ TokenGSInstanceName }
  const                     { TokenWith _ TokenConst }
  manifest                  { TokenWith _ TokenManifest }
  extern                    { TokenWith _ TokenExtern }
  let                       { TokenWith _ TokenLet }
  in                        { TokenWith _ TokenIn }
  fun                       { TokenWith _ TokenFun }
  if                        { TokenWith _ TokenIf }
  then                      { TokenWith _ TokenThen }
  else                      { TokenWith _ TokenElse }
  begin                     { TokenWith _ TokenBegin }
  end                       { TokenWith _ TokenEnd }
  function                  { TokenWith _ TokenFunction }

%right in
%nonassoc "==" "!=" '<' "<=" '>' ">="
%left "||"
%left "&&"
%left '+' '-'
%left '*' '/'
%left "**"
%left NEG POS
%right '.'

%%

stmts : stmt                                     { $1 }
      | stmts ';' stmt                           { $1 <> $3 }

stmt : specialDecl                               { $1 }
     | functionDef                               { $1 }
     | decl                                      { $1 }

specialDecl : dimension "::" int                 { [SpcDecl (getPos $1) (Dimension $3)] }
            | axes "::" axesLabels               { [SpcDecl (getPos $1) (Axes $3)] }
            | grid_struct_type_name "::" var     { [SpcDecl (getPos $1) (GSTypeName $3)] }
            | grid_struct_instance_name "::" var { [SpcDecl (getPos $1) (GSInstanceName $3)] }

axesLabels : var                                 { [$1] }
           | axesLabels ',' var                  { $1 <> [$3] }

functionDef : begin function rexp '=' var '(' args ')' ';' decls ';' end function { [TypeDecl (getPos $1) (ModifiedType [] None) (Ident $5), VarDecl (getPos $1) (Ident $5) (Lambda' $7 (Let' $10 $3)) ] }

decls : decl                                     { $1 }
      | decls ';' decl                           { $1 <> $3 }

decl : texp "::" exps                            { map (unwrapExp $1 (getPos $2)) $3 }
     | exp '=' rexp                              { [VarDecl (getPos $2) $1 $3] }
     |                                           { [] }

exps : exp0                                      { $1 }
     | exps ',' exp0                             { $1 <> $3 }

exp0 : exp                                       { [Left $1] }
     | exp '=' rexp                              { [Left $1, Right (VarDecl (getPos $2) $1 $3)] }

exp : var                                        { Ident $1 }
    | function                                   { Ident "function" }
    | '(' tupleOfexp ')'                         { Tuple $2 }
    | var '[' nPlusK ']'                         { Grid $3 (Ident $1) }

tupleOfexp : exp                                 { [$1] }
           | tupleOfexp ',' exp                  { $1 <> [$3] }

nPlusK : nPlusK0                                 { $1 }
       | nPlusK ',' nPlusK0                      { $1 <> $3 }

nPlusK0 :                                        { [] }
        | var                                    { [NPlusK $1 0] }
        | rat                                    { [NPlusK "" $1] }
        | '+' rat                                { [NPlusK "" $2] }
        | '-' rat                                { [NPlusK "" (negate $2)] }
        | var '+' rat                            { [NPlusK $1 $3] }
        | var '-' rat                            { [NPlusK $1 (negate $3)] }

rat : rat '+' rat                                { $1 + $3 }
    | rat '-' rat                                { $1 - $3 }
    | rat '*' rat                                { $1 * $3 }
    | rat '/' rat                                { $1 / $3 }
    | numExp                                     { $1 }

numExp : '(' rat ')'                             { $2 }
       | int                                     { toRational $1 }
       | float                                   { toRational $1 }

texp : tmods exp                                 { ModifiedType $1 $2 }
     | tmods                                     { ModifiedType $1 None }
     | exp                                       { ModifiedType [] $1 }

tmods : tmod                                     { $1 }
      | tmods tmod                               { $1 <> $2 }

tmod : const                                     { [TMConst] }
     | manifest                                  { [TMManifest] }
     | extern                                    { [TMExtern] }

rexp : rexp '+' rexp                             { Binop' Add $1 $3 }
     | rexp '-' rexp                             { Binop' Sub $1 $3 }
     | rexp '*' rexp                             { Binop' Mul $1 $3 }
     | rexp '/' rexp                             { Binop' Div $1 $3 }
     | rexp "**" rexp                            { Binop' Pow $1 $3 }
     | rexp "||" rexp                            { Binop' Or $1 $3 }
     | rexp "&&" rexp                            { Binop' And $1 $3 }
     | rexp "==" rexp                            { Binop' Eq $1 $3 }
     | rexp "!=" rexp                            { Binop' NEq $1 $3 }
     | rexp '<' rexp                             { Binop' Lt $1 $3 }
     | rexp "<=" rexp                            { Binop' LEq $1 $3 }
     | rexp '>' rexp                             { Binop' Gt $1 $3 }
     | rexp ">=" rexp                            { Binop' GEq $1 $3 }
     | '-' rexp %prec NEG                        { Uniop' Minus $2 }
     | '+' rexp %prec POS                        { Uniop' Plus $2 }
     | term                                      { $1 }
     | let decls in rexp                         { Let' $2 $4 }
     | let decls ';' in rexp                     { Let' $2 $5 }
     | fun '(' args ')' rexp                     { Lambda' $3 $5 }
     | if rexp then rexp else rexp               { If' $2 $4 $6 }
     | apply                                     { $1 }
     | rexp '.' rexp                               { Lambda' [Ident "x"] (App' $1 (App' $3 (Ident' "x"))) }

term : '(' rexp ')'                              { $2 }
     | var                                       { Ident' $1 }
     | int                                       { Imm' (toRational $1) }
     | float                                     { Imm' (toRational $1) }
     | tuple                                     { $1 }
     | var '[' nPlusK ']'                        { Grid' $3 (Ident' $1) }

args :                                           { [] }
     | arg                                       { [$1] }
     | args ',' arg                              { $1 <> [$3] }

arg : var                                        { Ident $1 }
    | '(' args ')'                               { Tuple $2 }

apply : term terms                                { applyChain $1 $2 }

terms : term                                     { [$1] }
      | terms term                               { $1 <> [$2] }

tuple : '(' rexp ',' restOftuple ')'             { Tuple' ([$2] <> $4) }

restOftuple : rexp                               { [$1] }
            | restOftuple ',' rexp               { $1 <> [$3] }


