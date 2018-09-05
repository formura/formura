{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Formura.Generator.Functions where

import Control.Lens
import Control.Monad.Trans
import Control.Monad.Writer
import Data.List (intercalate)
import Data.Traversable (for)
import Text.Printf (printf)

import Formura.NumericalConfig
import Formura.GlobalEnvironment
import Formura.OrthotopeMachine.Graph
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

defGlobalFunction :: IsGen m => String -> [(CType, String)] -> CType -> ([CVariable] -> BuildM m a) -> m a
defGlobalFunction = defFunction globalFunctions

defLocalFunction :: IsGen m => String -> [(CType, String)] -> CType -> ([CVariable] -> BuildM m a) -> m a
defLocalFunction = defFunction localFunctions

defFunction :: IsGen m => (Lens' CodeStructure [CFunction]) -> String -> [(CType, String)] -> CType -> ([CVariable] -> BuildM m a) -> m a
defFunction setter name args rt body = do
  let as = [CVariable n t Nothing Nothing | (t,n) <- args]
  (res, stmts) <- build $ body as
  setter %= (++ [CFunction name rt args stmts])
  return res

loop :: IsGen m => [Int] -> ([String] -> BuildM m ()) -> BuildM m ()
loop size body = do
  let idxNames = ["i" ++ show i | i <- [1..(length size)]]
  let idx = [(n, 0, s, 1) | (n,s) <- zip idxNames size]
  (_, stmt) <- lift $ build $ body idxNames
  tell [Loop idx stmt]
  return ()

getSize :: CType -> [Int]
getSize (CPtr t) = getSize t
getSize (CArray s _) = s
getSize (CStruct _ ((_,(CArray s _)):_)) = s
getSize _ = error "Error at Formura.Generator.Functions.getSize" 

getFields :: CVariable -> [String]
getFields (CVariable _ t _ _) =
  case t of
    (CArray _ (CStruct _ fs)) -> map fst fs
    (CStruct _ fs) -> map fst fs
    _ -> error "Error at Formura.Generator.Functions.getFields"

mkIdent :: String -> CVariable -> [String] -> [Int] -> String
mkIdent f (CVariable n t _ _) idx offset
  | isSoA t = n ++ "." ++ f ++ formatIdx idx offset
  | isSoA' t = n ++ "->" ++ f ++ formatIdx idx offset
  | isAoS t = n ++ formatIdx idx offset ++ "." ++ f
  | otherwise = error "Error at Formura.Generator.Functions.mkIdent"
  where
    isArray (CArray _ _) = True
    isArray _ = False
    isSoA (CStruct _ fs) = all (isArray . snd) fs
    isSoA _ = False
    isSoA' (CPtr t) = isSoA t
    isSoA' _ = False
    isAoS (CArray _ (CStruct _ _)) = True
    isAoS _ = False

formatIdx :: [String] -> [Int] -> String
formatIdx is os = concat ["[" ++ i ++ formatInt o ++ "]" | (i,o) <- zip is os]

formatInt :: Int -> String
formatInt x = if x == 0 then "" else printf "%+d" x

(@=) :: String -> String -> CStatement
l @= r = Bind l r

infixl 1 @=

copy :: IsGen m => CVariable -> CVariable -> [Int] -> [Int] -> BuildM m ()
copy src tgt srcOffset tgtOffset = do
  let s = zipWith min (getSize $ variableType src) (getSize $ variableType tgt)
  loop s $ \idx -> do
    let fs = getFields src
    tell [mkIdent f tgt idx tgtOffset @= mkIdent f src idx srcOffset | f <- fs, f `elem` (getFields tgt)]
  return ()

formatRank :: [Int] -> String
formatRank b = intercalate "_" [f i | i <- b]
  where f x | x == 0 = "0"
            | x == 1 = "p1"
            | x == -1 = "m1"

sendrecv :: IsGen m => [(String,CType)] -> CVariable -> CVariable -> Int -> BuildM m ()
sendrecv gridStruct src tgt s = do
  bases <- view (omGlobalEnvironment . commBases)
  gridPerNode <- view (omGlobalEnvironment . envNumericalConfig . icGridPerNode)
  commType <- defLocalTypeStruct "Formura_Comm_Buf" gridStruct Normal
  (sendReqs, recvReqs, recvBufs) <- fmap unzip3 $ for bases $ \b -> do
    let r = formatRank b
    sendbuf <- declLocalVariable (Just "static") (CArray [if d == 1 then 2*s else n | (d,n) <- zip b gridPerNode] commType) ("send_buf_" ++ r) Nothing
    copy src sendbuf [if d == 1 then n-2*s else 0 | (d,n) <- zip b gridPerNode] (repeat 0)
    sendReq <- sendTo r sendbuf
    let r' = formatRank $ map negate b
    recvbuf <- declLocalVariable (Just "static") (CArray [if d == 1 then 2*s else n | (d,n) <- zip b gridPerNode] commType) ("recv_buf_" ++ r') Nothing
    recvReq <- recvFrom r' recvbuf
    return (sendReq, recvReq, recvbuf)
  mapM_ wait sendReqs
  mapM_ wait recvReqs
  sequence_ [copy recvBuf tgt (repeat 0) [if d == 1 then 0 else 2*s | (d,n) <- zip b gridPerNode] | (b,recvBuf) <- zip bases recvBufs]

sendTo :: IsGen m => String -> CVariable -> BuildM m CVariable
sendTo r v = do
  req <- declLocalVariable Nothing (CRawType "MPI_Request") ("send_req_" ++ r) Nothing
  call "MPI_Isend" [ref v, "sizeof(" ++ variableName v ++ ")", "MPI_BYTE", "n->rank_" ++ r, "0", "n->mpi_world", ref req]
  return req

recvFrom :: IsGen m => String -> CVariable -> BuildM m CVariable
recvFrom r v = do
  req <- declLocalVariable Nothing (CRawType "MPI_Request") ("recv_req_" ++ r) Nothing
  call "MPI_Irecv" [ref v, "sizeof(" ++ variableName v ++ ")", "MPI_BYTE", "n->rank_" ++ r, "0", "n->mpi_world", ref req]
  return req

wait :: IsGen m => CVariable -> BuildM m ()
wait v = call "MPI_Wait" [ref v, "MPI_STATUS_IGNORE"]

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
