{
module Formura0.Frontend.Lexer where

import Data.Text (Text, pack)
import Data.Scientific
}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]
$nl = [\n\r\f]
$whitespace = $white # $nl

@decimal = $digit+
@exponent = [eE] [\-\+]? @decimal
@float = @decimal \. @decimal @exponent? | @decimal @exponent

tokens :-
  $whitespace+                  ;
  \#~$nl*                       ;
  [$nl \;]+                     { withPos TokenEOS }
  =                             { withPos TokenEq }
  \.                            { withPos TokenCom }
  \*\*                          { withPos TokenPow }
  \+                            { withPos TokenAdd }
  \-                            { withPos TokenSub }
  \*                            { withPos TokenMul }
  \/                            { withPos TokenDiv }
  \,                            { withPos TokenSep }
  \(                            { withPos TokenOP }
  \)                            { withPos TokenCP }
  \[                            { withPos TokenOB }
  \]                            { withPos TokenCB }
  "::"                          { withPos TokenTypeSep }
  "&&"                          { withPos TokenAnd }
  "||"                          { withPos TokenOr }
  "=="                          { withPos TokenEQ }
  "!="                          { withPos TokenNEQ }
  "<"                           { withPos TokenLT }
  "<="                          { withPos TokenLE }
  ">"                           { withPos TokenGT }
  ">="                          { withPos TokenGE }
  dimension                     { withPos TokenDim }
  axes                          { withPos TokenAxes }
  grid_struct_type_name         { withPos TokenGSTypeName }
  grid_struct_instance_name     { withPos TokenGSInstanceName }
  const                         { withPos TokenConst }
  manifest                      { withPos TokenManifest }
  extern                        { withPos TokenExtern }
  let                           { withPos TokenLet }
  in                            { withPos TokenIn }
  fun                           { withPos TokenFun }
  if                            { withPos TokenIf }
  then                          { withPos TokenThen }
  else                          { withPos TokenElse }
  begin                         { withPos TokenBegin }
  end                           { withPos TokenEnd }
  function                      { withPos TokenFunction }
  $digit+                       { readWithPos TokenInt }
  @float              { readWithPos TokenFloat }
  $alpha [$alpha $digit \_ \']* { scanWithPos TokenVar }

{
data Token = TokenInt Int
           | TokenFloat Scientific
           | TokenVar Text
           | TokenEq
           | TokenCom
           | TokenAdd
           | TokenSub
           | TokenMul
           | TokenDiv
           | TokenPow
           | TokenSep
           | TokenAnd
           | TokenOr
           | TokenEQ
           | TokenNEQ
           | TokenLT
           | TokenLE
           | TokenGT
           | TokenGE
           | TokenOP
           | TokenCP
           | TokenOB
           | TokenCB
           | TokenTypeSep
           | TokenDim
           | TokenAxes
           | TokenGSTypeName
           | TokenGSInstanceName
           | TokenConst
           | TokenManifest
           | TokenExtern
           | TokenLet
           | TokenIn
           | TokenFun
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenBegin
           | TokenEnd
           | TokenFunction
           | TokenEOS
           -- ^ end of statement
           | TokenEOF
  deriving (Eq,Show)

data TokenWithPos = TokenWith AlexPosn Token
  deriving (Eq,Show)

withPos' :: (String -> Token) -> AlexInput -> Int -> Alex TokenWithPos
withPos' f (p, _, _, input) len = return $ TokenWith p (f (take (fromIntegral len) input))

withPos :: Token -> AlexInput -> Int -> Alex TokenWithPos
withPos x = withPos' (\_ -> x)

readWithPos :: Read a => (a -> Token) -> AlexInput -> Int -> Alex TokenWithPos
readWithPos x = withPos' (\s -> x (read s))

scanWithPos :: (Text -> Token) -> AlexInput -> Int -> Alex TokenWithPos
scanWithPos x = withPos' (\s -> x (pack s))

getPos :: TokenWithPos -> AlexPosn
getPos (TokenWith p _) = p

alexEOF :: Alex TokenWithPos
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ TokenWith p TokenEOF
}
