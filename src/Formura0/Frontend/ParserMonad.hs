module Formura0.Frontend.ParserMonad where

import Formura0.Frontend.Lexer

type Parser a = Alex a

thenP :: Parser a -> (a -> Parser b) -> Parser b
thenP = (>>=)

returnP :: a -> Parser a
returnP = return

alexShowError :: (Show t, Show t1) => (t, t1) -> Alex a
alexShowError (line, column) = alexError $ "error at " ++ (show (line, column))

alexGetPosition :: Alex (AlexPosn)
alexGetPosition = Alex $ \s@AlexState{alex_pos=pos} -> Right (s, pos)

happyError :: Parser a
happyError = do
  (AlexPn _ line col) <- alexGetPosition
  alexShowError (line, col)

lexer :: (TokenWithPos -> Parser a) -> Parser a
lexer f = alexMonadScan >>= f

