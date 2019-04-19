{
{-# LANGUAGE OverloadedStrings #-}
module Formura0.Frontend.Lexer where

import qualified Data.Text as T
import Data.Scientific
}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+;
  "#".*;
  let                           { withPos TokenLet }
  in                            { withPos TokenIn }
  end                           { withPos TokenEnd }
  fun                           { withPos TokenFun }
  const                         { withPos TokenConst }
  extern                        { withPos TokenExtern }
  manifest                      { withPos TokenManifest }
  $digit+                       { readWithPos TokenImm }
  $digit+\.$digit+              { readWithPos TokenImm }
  \+                            { withPos TokenAdd }
  \-                            { withPos TokenSub }
  \*                            { withPos TokenMul }
  \/                            { withPos TokenDiv }
  =                             { withPos TokenEq }
  \,                            { withPos TokenSep }
  \(                            { withPos TokenOP }
  \)                            { withPos TokenCP }
  ::                            { withPos TokenTypeSep }
  $alpha [$alpha $digit \_ \']* { scanWithPos TokenVar }

{

data TokenWithPos = TokenWith AlexPosn Token

data Token = TokenLet
           | TokenIn
           | TokenEnd
           | TokenFun
           -- ^ function (lambda)
           | TokenAdd
           | TokenSub
           | TokenMul
           | TokenDiv
           | TokenEq
           | TokenSep
           | TokenTypeSep
           | TokenOP
           -- ^ open parens
           | TokenCP
           -- ^ close parens
           | TokenVar T.Text
           | TokenImm Scientific
           -- ^ immediate
           | TokenConst
           | TokenExtern
           | TokenManifest
           | TokenEOF

withPos' f (p, _, _, input) len = return $ TokenWith p (f (take (fromIntegral len) input))

withPos x = withPos' (\_ -> x)

readWithPos x = withPos' (\s -> x (read s))

scanWithPos x = withPos' (\s -> x (T.pack s))

getPos :: TokenWithPos -> AlexPosn
getPos (TokenWith p _) = p

alexEOF :: Alex TokenWithPos
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ TokenWith p TokenEOF

}

