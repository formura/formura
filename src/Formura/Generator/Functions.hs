{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Formura.Generator.Functions where

import Control.Lens
import Control.Monad.Trans
import Control.Monad.Writer
import Text.Printf

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
  tell [Decl v]
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

loop :: IsGen m => [Int] -> ([String] -> BuildM m ()) -> BuildM m ()
loop size body = do
  let idxNames = ["i" ++ show i | i <- [1..(length size)]]
  let idx = [(n, 0, s, 1) | (n,s) <- zip idxNames size]
  (_, stmt) <- lift $ build $ body idxNames
  tell [Loop idx stmt]
  return ()

getSize :: CVariable -> [Int]
getSize (CVariable _ t _ _) =
  case t of
    (CArray s (CStruct _ _)) -> s
    (CStruct _ ((_,(CArray s _)):_)) -> s
    _ -> error "Error at Formura.Generator.Functions.getSize"

getFields :: CVariable -> [String]
getFields (CVariable _ t _ _) =
  case t of
    (CArray _ (CStruct _ fs)) -> map fst fs
    (CStruct _ fs) -> map fst fs
    _ -> error "Error at Formura.Generator.Functions.getFields"

mkIdent :: String -> CVariable -> [String] -> [Int] -> String
mkIdent f (CVariable n t _ _) idx offset
  | isSoA t = n ++ "." ++ f ++ mkIdx idx offset
  | isSoA' t = n ++ "->" ++ f ++ mkIdx idx offset
  | isAoS t = n ++ mkIdx idx offset ++ "." ++ f
  | otherwise = error "Error at Formura.Generator.Functions.mkIdent"
  where
    isArray (CArray _ _) = True
    isArray _ = False
    isSoA (CStruct _ fs) = all (isArray . snd) fs
    isSoA _ = False
    isSoA' (CPtr t) = isSoA t
    isAoS (CArray _ (CStruct _ _)) = True
    isAoS _ = False
    show' x = if x == 0 then "" else printf "%+d" x
    mkIdx is os = concat ["[" ++ i ++ show' o ++ "]" | (i,o) <- zip is os]

(@=) :: String -> String -> CStatement
l @= r = Bind l r

infixl 1 @=

copy :: IsGen m => CVariable -> CVariable -> [Int] -> [Int] -> BuildM m ()
copy src tgt srcOffset tgtOffset = do
  let s = zipWith min (getSize src) (getSize tgt)
  loop s $ \idx -> do
    let fs = getFields src
    tell [mkIdent f tgt idx tgtOffset @= mkIdent f src idx srcOffset | f <- fs, f `elem` (getFields tgt)]
  return ()

sendrecv :: IsGen m => CVariable -> Int -> BuildM m CVariable
sendrecv v s = do
  return v

call :: IsGen m => String -> [String] -> BuildM m ()
call fn args = do
  tell [Call fn args]
  return ()

ref :: CVariable -> String
ref (CVariable n t _ _) | isArray t = n
                        | isPtr t = n
                        | otherwise = "&" ++ n
  where
    isArray (CArray _ _) = True
    isArray _ = False
    isPtr (CPtr _) = True
    isPtr _ = False

raw :: IsGen m => String -> BuildM m ()
raw c = do
  tell [Raw c]
  return ()
