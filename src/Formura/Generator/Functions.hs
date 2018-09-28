{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module Formura.Generator.Functions where

import Control.Lens
import Control.Monad.Trans
import Control.Monad.Writer
import Data.Maybe (isJust)
import Data.List (intercalate)
import Data.Traversable (for)
import Text.Printf (printf)

import Formura.NumericalConfig
import Formura.GlobalEnvironment
import Formura.OrthotopeMachine.Graph
import Formura.Generator.Types

addHeader :: IsGen m => String -> m ()
addHeader h = headers %= (++ ["#include " ++ h])

defineParam :: IsGen m => String -> String -> m ()
defineParam n v = definedParams %= (++ ["#define " ++ n ++ " " ++ v])

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
  where wrap s0 (CArray _ t) = CArray s0 t
        wrap s0 t = CArray s0 t

defTypeStruct :: IsGen m => (Lens' CodeStructure [CTypedef]) -> String -> [(String, CType)] -> Kind -> m CType
defTypeStruct setter n fs k = do
  let fs' = redeftype k fs
  let t = case k of
            Normal -> CStruct n fs'
            AoS s -> CArray s (CStruct n fs')
            SoA _ -> CStruct n fs'
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

loop :: IsGen m => [Int] -> (Idx -> BuildM m ()) -> BuildM m ()
loop size body = do
  let idx = [("i" ++ show i, 0, s, 1) | (i,s) <- zip [1..(length size)] size]
  let d = length size
  withOmp <- view (omGlobalEnvironment . envNumericalConfig . icWithOmp)
  when (withOmp > 0) $ raw ("#pragma omp parallel for" ++ if d > 1 then " collapse(" ++ show d ++ ")" else "")
  loopWith idx body

loopWith :: IsGen m => [(String,Int,Int,Int)] -> (Idx -> BuildM m ()) -> BuildM m ()
loopWith idx body = do
  (_, stmt) <- lift $ build $ body $ toIdx [n | (n,_,_,_) <- idx]
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

mkIdent :: String -> CVariable -> Idx -> String
mkIdent f (CVariable n t _ _) idx
  | isSoA t = n ++ "." ++ f ++ show idx
  | isSoA' t = n ++ "->" ++ f ++ show idx
  | isAoS t = n ++ show idx ++ "." ++ f
  | otherwise = error "Error at Formura.Generator.Functions.mkIdent"
  where
    isArray (CArray _ _) = True
    isArray _ = False
    isSoA (CStruct _ fs) = all (isArray . snd) fs
    isSoA _ = False
    isSoA' (CPtr t0) = isSoA t0
    isSoA' _ = False
    isAoS (CArray _ (CStruct _ _)) = True
    isAoS _ = False

formatInt :: Int -> String
formatInt x = if x == 0 then "" else printf "%+d" x

(@=) :: String -> String -> CStatement
l @= r = Bind l r

infixl 1 @=

newtype Idx = Idx { fromIdx :: [String] }

class IsIdx a where
  toIdx :: a -> Idx

instance IsIdx Idx where
  toIdx = id

instance IsIdx [String] where
  toIdx i = Idx i

instance IsIdx [Int] where
  toIdx is = Idx [formatInt i | i <- is]

instance Show Idx where
  show (Idx is) = concat ["[" ++ i ++ "]" | i <- is]

-- instance Semigroup Idx where
--   (Idx is) <> (Idx is') = Idx (zipWith (++) is is')

instance Monoid Idx where
  mempty = toIdx @[Int] (repeat 0)
  mappend (Idx is) (Idx is') = Idx (zipWith (++) is is')

(><) :: Idx -> Idx -> Idx
(Idx is) >< (Idx is') = Idx (is ++ is')

infixr 1 ><

empty :: Idx
empty = mempty

copy :: (IsGen m, IsIdx a, IsIdx b) => CVariable -> CVariable -> a -> b -> BuildM m ()
copy src tgt srcOffset tgtOffset = do
  let s = zipWith min (getSize $ variableType src) (getSize $ variableType tgt)
  loop s $ \idx -> do
    let fs = getFields src
    tell [mkIdent f tgt (idx <> toIdx tgtOffset) @= mkIdent f src (idx <> toIdx srcOffset) | f <- fs, f `elem` (getFields tgt)]
  return ()

formatRank :: [Int] -> String
formatRank b = intercalate "_" [f i | i <- b]
  where f x | x == 0 = "0"
            | x == 1 = "p1"
            | x == -1 = "m1"
            | otherwise = error "Error in Formura.Generator.Functions.formatRank"

sendrecv :: IsGen m => String -> [(String,CType)] -> CVariable -> CVariable -> Int -> BuildM m ()
sendrecv ident gridStruct src tgt s = do
  rs <- isendrecv ident gridStruct src s
  waitAndCopy rs tgt s

isendrecv :: IsGen m => String -> [(String,CType)] -> CVariable -> Int -> BuildM m ([CVariable],[CVariable],[CVariable])
isendrecv ident gridStruct src s = do
  mmpiShape <- view (omGlobalEnvironment . envNumericalConfig . icMPIShape)
  bases <- view (omGlobalEnvironment . commBases)
  gridPerNode <- view (omGlobalEnvironment . envNumericalConfig . icGridPerNode)
  commType <- defLocalTypeStruct ("Formura_Comm_Buf" ++ ident) gridStruct Normal
  fmap unzip3 $ for bases $ \b -> do
    let r = formatRank b
    let r' = formatRank $ map negate b
    sendbuf <- declLocalVariable (Just "static") (CArray [if d == 1 then 2*s else n | (d,n) <- zip b gridPerNode] commType) ("send_buf" ++ ident ++ "_" ++ r) Nothing
    recvbuf <- declLocalVariable (Just "static") (CArray [if d == 1 then 2*s else n | (d,n) <- zip b gridPerNode] commType) ("recv_buf" ++ ident ++ "_" ++ r') Nothing
    copy src sendbuf [if d == 1 then n-2*s else 0 | (d,n) <- zip b gridPerNode] empty
    (sendReq, recvReq) <- case mmpiShape of
                            Nothing -> copy sendbuf recvbuf empty empty >> return (sendbuf,recvbuf) -- この返り値はよくないが妥協した
                            Just _ -> (,) <$> sendTo r sendbuf <*> recvFrom r' recvbuf
    -- sendReq <- sendTo r sendbuf
    -- recvReq <- recvFrom r' recvbuf
    return (sendReq, recvReq, recvbuf)

waitAndCopy :: IsGen m => ([CVariable],[CVariable],[CVariable]) -> CVariable -> Int -> BuildM m ()
waitAndCopy (sendReqs,recvReqs,recvBufs) tgt s = do
  bases <- view (omGlobalEnvironment . commBases)
  mmpiShape <- view (omGlobalEnvironment . envNumericalConfig . icMPIShape)
  when (isJust mmpiShape) $ do
    mapM_ wait sendReqs
    mapM_ wait recvReqs
  sequence_ [copy recvBuf tgt empty [if d == 1 then 0 else 2*s | d <- b] | (b,recvBuf) <- zip bases recvBufs]

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

statement :: IsGen m => String -> BuildM m ()
statement s = raw $ s ++ ";"
