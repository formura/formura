module Formura0.Utils where

import Formura0.Frontend.Lexer

formatPos :: AlexPosn -> String
formatPos (AlexPn _ l c) = "line " ++ show l ++ ", column " ++ show c ++ ":"
