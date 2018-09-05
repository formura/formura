{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Formura.Generator.Encode where

import Control.Lens
import Data.List
import Text.Printf

import Formura.Generator.Types

render :: CodeStructure -> (String, String)
render cs = (hContent, cContent)
  where
    encodesWith f getter = map f (cs ^. getter)
    hContent = unlines $ encodesWith encodeH headers
                      ++ encodesWith encodeH globalTypes
                      ++ encodesWith encodeH globalVariables
                      ++ encodesWith encodeH globalFunctions

    cContent = unlines $ ["#include" <+> show (cs ^. hFileName)]
                      ++ encodesWith encode localTypes
                      ++ encodesWith encode globalVariables
                      ++ encodesWith encode localFunctions
                      ++ encodesWith encode globalFunctions

(<+>) :: String -> String -> String
a <+> b = a ++ " " ++ b

(<+|) :: String -> String -> String
a <+| b = a ++ " " ++ b ++ ";"

infixr 5 <+>
infixr 6 <+|

class EncodeH a where
  encodeH :: a -> String

instance EncodeH String where
  encodeH = id

instance EncodeH CVariable where
  encodeH (CVariable n t _ _) = "extern" <+> encode (CVariable n t Nothing Nothing)

instance EncodeH CTypedef where
  encodeH (CTypedef org als) = "typedef" <+> encode org <+| als
  encodeH (CTypedefStruct fs t) = "typedef struct {\n" ++ unlines [encode (CVariable n t Nothing Nothing) | (n,t) <- fs] ++  "}" <+| t

instance EncodeH CFunction where
  encodeH (CFunction fn rt ag _) = encode rt <+> fn ++ parens (map (encode . fst) ag) ++ ";"

class Encode a where
  encode :: a -> String

instance Encode [Int] where
  encode ns = concat ["[" ++ show n ++ "]" | n <- ns]

instance Encode CVariable where
  encode (CVariable n t mv ml) = let with (Just l) s = l <+> s
                                     with Nothing s = s

                                     decl (CArray s t) n = encode t <+> n ++ encode s
                                     decl t n = encode t <+> n
                                  in case mv of
                                      (Just v) -> with ml (decl t n <+> "=" <+| v)
                                      Nothing -> with ml (decl t n ++ ";")

instance Encode CTypedef where
  encode = encodeH

instance Encode CFunction where
  encode (CFunction fn rt ag b) = encode rt <+> fn ++ parens [encode t <+> n | (t,n) <- ag] ++ " {\n"
                                ++ unlines (map encode b)
                                ++ "}"

instance Encode CType where
  encode CVoid = "void"
  encode CInt = "int"
  encode CFloat = "float"
  encode CDouble = "double"
  encode (CArray _ t) = encode t
  encode (CStruct n _) = n
  encode (CPtr t) = encode t <+> "*"
  encode (CRawType n) = n

instance Encode CStatement where
  encode (Decl v) = encode v
  encode (Bind lhs rhs) = lhs <+> "=" <+| rhs
  encode (Loop idx body) = foldr (\(n,i0,i1,di) acc -> printf "for(int %s = %d; %s < %d; %s += %d) {\n%s}\n" n i0 n i1 n di acc) (unlines $ map encode body) idx
  encode (Call fn args) = fn ++ parens args ++ ";"
  encode (Raw c) = c ++ ";"

parens :: [String] -> String
parens xs = "(" ++ intercalate "," xs ++ ")"

braces :: String -> String
braces body = "{" ++ body ++ "}"
