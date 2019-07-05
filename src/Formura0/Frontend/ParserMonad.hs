module Formura0.Frontend.ParserMonad where

import Formura0.Frontend.Lexer
import Formura0.Syntax

type Parser a = Alex a

thenP :: Parser a -> (a -> Parser b) -> Parser b
thenP = (>>=)

returnP :: a -> Parser a
returnP = return

alexGetPosition :: Alex AlexPosn
alexGetPosition = Alex $ \s@AlexState{alex_pos=pos} -> Right (s, pos)

happyError :: Parser a
happyError = do
  (AlexPn _ l c) <- alexGetPosition
  alexError $ "parse error at line " ++ show l ++ " column " ++ show c

lexer :: (TokenWithPos -> Parser a) -> Parser a
lexer f = alexMonadScan >>= f

-- Utils
unwrapExp :: (ModifiedType Exp) -> AlexPosn -> Either Exp Statement0 -> Statement0
unwrapExp t p (Left e)  = TypeDecl p t e
unwrapExp _ _ (Right d) = d

applyChain :: Exp' -> [Exp'] -> Exp'
applyChain _ []     = error "error: empty list"
applyChain e (a:as) = foldl (\acc arg -> App' acc arg) (App' e a) as
