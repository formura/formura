{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Formura.Generator.Functions where

import Control.Lens
import Control.Monad.Writer

import Formura.Generator.Types
import Formura.Generator.Encode


addHeader :: IsGen m => String -> m ()
addHeader h = headers %= (++ ["#include " ++ h])

defType :: IsGen m => (Lens' CodeStructure [CTypedef]) -> String -> CType -> m CType
defType setter als org = do
  setter %= (++ [CTypedef org als])
  return (CRawType als)

defGlobalType :: IsGen m => String -> CType -> m CType
defGlobalType = defType globalTypes

defLocalType :: IsGen m => String -> CType -> m CType
defLocalType = defType localTypes

redeftype :: Kind -> [(String,CType)] -> [(String,CType)]
redeftype Normal fs = fs
redeftype (AoS _) fs = [(n, unwrap t) | (n, t) <- fs]
  where unwrap (CArray _ t) = t
        unwrap t = t
redeftype (SoA s) fs = [(n, wrap s t) | (n, t) <- fs]
  where wrap s (CArray _ t) = CArray s t
        wrap s t = CArray s t

defTypeStruct :: IsGen m => (Lens' CodeStructure [CTypedef]) -> String -> [(String, CType)] -> Kind -> m CType
defTypeStruct setter n fs k = do
  let fs' = redeftype k fs
  let t = case k of
            Normal -> CStruct n fs'
            AoS s -> CArray s (CStruct n fs')
            SoA s -> CStruct n fs'
  setter %= (++ [CTypedefStruct fs' n])
  return t

defGlobalTypeStruct :: IsGen m => String -> [(String, CType)] -> Kind -> m CType
defGlobalTypeStruct = defTypeStruct globalTypes

defLocalTypeStruct :: IsGen m => String -> [(String, CType)] -> Kind -> m CType
defLocalTypeStruct = defTypeStruct localTypes

declGlobalVariable :: IsGen m => CType -> String -> Maybe String -> m CVariable
declGlobalVariable  t n mv = do
  let v = CVariable n t mv Nothing
  globalVariables %= (++ [v]) 
  return v

declLocalVariable :: IsGen m => Maybe String -> CType -> String -> Maybe String -> BuildM m CVariable
declLocalVariable ml t n mv = do
  let v = CVariable n t mv ml
  raw (encode v)
  return v

defGlobalFunction :: IsGen m => String -> [(CType, String)] -> CType -> BuildM m a -> m a
defGlobalFunction = defFunction globalFunctions

defLocalFunction :: IsGen m => String -> [(CType, String)] -> CType -> BuildM m a -> m a
defLocalFunction = defFunction localFunctions

defFunction :: IsGen m => (Lens' CodeStructure [CFunction]) -> String -> [(CType, String)] -> CType -> BuildM m a -> m a
defFunction setter name args rt body = do
  (res, stmts) <- build body
  setter %= (++ [CFunction name rt args stmts])
  return res

copy :: IsGen m => CVariable -> CVariable -> [Int] -> [Int] -> BuildM m ()
copy src tgt srcOffset tgtOffset = do
  tell [Copy]
  return ()

sendrecv :: IsGen m => CVariable -> Int -> BuildM m CVariable
sendrecv v s = do
  tell [Sendrecv]
  -- FIX ME
  return v

call :: IsGen m => String -> [String] -> BuildM m ()
call fn args = do
  tell [Call]
  return ()

ref :: CVariable -> String
ref = undefined

raw :: IsGen m => String -> BuildM m ()
raw c = do
  tell [Raw]
  return ()
