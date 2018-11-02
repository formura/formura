{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module Formura.Generator.Functions where

import Control.Lens
import Control.Monad
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (maybeToList)
import Data.List (intercalate, sort, nub)
import Data.Traversable (for)
import Text.Printf (printf)

import Formura.NumericalConfig
import Formura.GlobalEnvironment
import Formura.OrthotopeMachine.Graph
import Formura.Generator.Types

withMPI :: ([Int] -> BuildM ()) -> BuildM ()
withMPI f = do
  mmpiShape <- view (globalEnvironment . envNumericalConfig . icMPIShape)
  case mmpiShape of
    Nothing -> return ()
    Just mpiShape -> f mpiShape

withOMP :: BuildM () -> BuildM ()
withOMP f = do
  omp <- view (globalEnvironment . envNumericalConfig . icWithOmp)
  when (omp > 0) $ f

withFirstStep :: (MMGraph -> BuildM ()) -> BuildM ()
withFirstStep f = do
  mfirstStepGraph <- view omFirstStepGraph
  case mfirstStepGraph of
    Nothing -> return ()
    Just g -> f g

withFilter :: (MMGraph -> BuildM ()) -> BuildM ()
withFilter f = do
  mfilterGraph <- view omFilterGraph
  case mfilterGraph of
    Nothing -> return ()
    Just g -> f g

getBlockOffsets :: BuildM [(CType,String)]
getBlockOffsets = do
  dim <- view (globalEnvironment . dimension)
  return [(CInt, "block_offset_" ++ show i) | i <- [1..dim]]

getSleeves :: BuildM [Int]
getSleeves = do
  step <- view (globalEnvironment . envNumericalConfig . icSleeve)
  first <- maybeToList <$> view (globalEnvironment . envNumericalConfig . icSleeve0)
  filter' <- maybeToList <$> view (globalEnvironment . envNumericalConfig . icFilterSleeve)
  bt <- view (globalEnvironment . envNumericalConfig . icBlockingType)
  let s = case bt of
           NoBlocking -> [step]
           TemporalBlocking _ _ nt -> [step*nt]
  return $ nub $ sort $ s ++ first ++ filter'

getSendBuf :: Int -> [Int] -> BuildM CVariable
getSendBuf s b =
  getVariable ("send_buf" ++ show s ++ "_" ++ formatRank b)

getRecvBuf :: Int -> [Int] -> BuildM CVariable
getRecvBuf s b =
  getVariable ("recv_buf" ++ show s ++ "_" ++ formatRank (map negate b))

scope :: BuildM a -> BuildM (a, [CStatement])
scope f = do
  st <- use stack
  stack .= []
  x <- f
  res <- use stack
  stack .= st
  return (x, res)

addVariable :: String -> CVariable -> BuildM ()
addVariable k v = variables %= HM.insert k v

getVariable :: String -> BuildM CVariable
getVariable k = do
  vs <- use variables
  case vs ^? ix k of
    Nothing -> error $ "Error: getVariable " ++ k
    Just v -> return v

getGlobalData :: BuildM CVariable
getGlobalData = do
  insntaceName <- view (globalEnvironment . gridStructInstanceName)
  getVariable insntaceName

addHeader :: String -> BuildM ()
addHeader h = scribe headers ["#include " ++ h]

defineParam :: String -> String -> BuildM ()
defineParam n v = scribe definedParams ["#define " ++ n ++ " " ++ v]

defType :: (Lens' CodeStructure [CTypedef]) -> String -> CType -> BuildM CType
defType setter als org = do
  scribe setter [CTypedef org als]
  return (CRawType als)

defGlobalType :: String -> CType -> BuildM CType
defGlobalType = defType globalTypes

defLocalType :: String -> CType -> BuildM CType
defLocalType = defType localTypes

redeftype :: Kind -> [(String,CType)] -> [(String,CType)]
redeftype Normal fs = fs
redeftype (AoS _) fs = [(n, unwrap t) | (n, t) <- fs]
  where unwrap (CArray _ t) = t
        unwrap t = t
redeftype (SoA s) fs = [(n, wrap s t) | (n, t) <- fs]
  where wrap s0 (CArray _ t) = CArray s0 t
        wrap s0 t = CArray s0 t

defTypeStruct :: (Lens' CodeStructure [CTypedef]) -> String -> [(String, CType)] -> Kind -> BuildM CType
defTypeStruct setter n fs k = do
  let fs' = redeftype k fs
  let t = case k of
            Normal -> CStruct n fs'
            AoS s -> CArray s (CStruct n fs')
            SoA _ -> CStruct n fs'
  scribe setter [CTypedefStruct fs' n]
  return t

defGlobalTypeStruct :: String -> [(String, CType)] -> Kind -> BuildM CType
defGlobalTypeStruct = defTypeStruct globalTypes

defLocalTypeStruct :: String -> [(String, CType)] -> Kind -> BuildM CType
defLocalTypeStruct = defTypeStruct localTypes

declVariable :: (Lens' CodeStructure [CVariable]) -> Maybe String -> CType -> String -> Maybe String -> BuildM CVariable
declVariable setter ml t n mv = do
  let v = CVariable n t mv ml
  scribe setter [v]
  addVariable n v
  return v

declGlobalVariable :: CType -> String -> Maybe String -> BuildM CVariable
declGlobalVariable = declVariable globalVariables Nothing

declLocalVariable :: Maybe String -> CType -> String -> Maybe String -> BuildM CVariable
declLocalVariable = declVariable localVariables

-- 関数内での変数宣言
declScopedVariable :: Maybe String -> CType -> String -> Maybe String -> BuildM CVariable
declScopedVariable ml t n mv = do
  let v = CVariable n t mv ml
  stack <>= [Decl v]
  return v

defGlobalFunction :: String -> [(CType, String)] -> CType -> ([CVariable] -> BuildM a) -> BuildM a
defGlobalFunction = defFunction globalFunctions

defLocalFunction :: String -> [(CType, String)] -> CType -> ([CVariable] -> BuildM a) -> BuildM a
defLocalFunction = defFunction localFunctions

defFunction :: (Lens' CodeStructure [CFunction]) -> String -> [(CType, String)] -> CType -> ([CVariable] -> BuildM a) -> BuildM a
defFunction setter name args rt body = do
  let as = [CVariable n t Nothing Nothing | (t,n) <- args]
  (res, stmts) <- scope $ body as
  scribe setter [CFunction name rt args stmts]
  return res

loop :: [Int] -> (Idx -> BuildM ()) -> BuildM ()
loop size body = do
  let idx = [("i" ++ show i, 0, s, 1) | (i,s) <- zip [1..(length size)] size]
  let d = length size
  withOMP $ raw ("#pragma omp parallel for" ++ if d > 1 then " collapse(" ++ show d ++ ")" else "")
  loopWith idx body

loopWith :: [(String,Int,Int,Int)] -> (Idx -> BuildM ()) -> BuildM ()
loopWith idx body = do
  (_, stmt) <- scope $ body $ toIdx [n | (n,_,_,_) <- idx]
  stack <>= [Loop idx stmt]
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

instance Semigroup Idx where
  (Idx is) <> (Idx is') = Idx (zipWith (++) is is')

instance Monoid Idx where
  mempty = toIdx @[Int] (repeat 0)

(><) :: Idx -> Idx -> Idx
(Idx is) >< (Idx is') = Idx (is ++ is')

infixr 1 ><

empty :: Idx
empty = mempty

copy :: (IsIdx a, IsIdx b) => CVariable -> CVariable -> a -> b -> BuildM ()
copy src tgt srcOffset tgtOffset = do
  let s = zipWith min (getSize $ variableType src) (getSize $ variableType tgt)
  loop s $ \idx -> do
    let fs = getFields src
    stack <>= [mkIdent f tgt (idx <> toIdx tgtOffset) @= mkIdent f src (idx <> toIdx srcOffset) | f <- fs, f `elem` (getFields tgt)]
  return ()

formatRank :: [Int] -> String
formatRank b = intercalate "_" [f i | i <- b]
  where f x | x == 0 = "0"
            | x == 1 = "p1"
            | x == -1 = "m1"
            | otherwise = error "Error in Formura.Generator.Functions.formatRank"

sendrecv :: CVariable -> CVariable -> Int -> BuildM ()
sendrecv src tgt s = do
  rs <- isendrecv src s
  waitAndCopy rs tgt s

isendrecv :: CVariable -> Int -> BuildM ([CVariable],[CVariable],[CVariable])
isendrecv src s = do
  mmpiShape <- view (globalEnvironment . envNumericalConfig . icMPIShape)
  bases <- view (globalEnvironment . commBases)
  gridPerNode <- view (globalEnvironment . envNumericalConfig . icGridPerNode)
  fmap unzip3 $ for bases $ \b -> do
    sendbuf <- getSendBuf s b
    recvbuf <- getRecvBuf s b
    copy src sendbuf [if d == 1 then n-2*s else 0 | (d,n) <- zip b gridPerNode] empty
    (sendReq, recvReq) <- case mmpiShape of
                            Nothing -> copy sendbuf recvbuf empty empty >> return (error "never eval",error "never eval") -- あまりよいデザインではない気が...
                            Just _ -> (,) <$> sendTo b sendbuf <*> recvFrom b recvbuf
    return (sendReq, recvReq, recvbuf)

waitAndCopy :: ([CVariable],[CVariable],[CVariable]) -> CVariable -> Int -> BuildM ()
waitAndCopy (sendReqs,recvReqs,recvBufs) tgt s = do
  bases <- view (globalEnvironment . commBases)
  withMPI $ \_ -> do
    mapM_ wait sendReqs
    mapM_ wait recvReqs
  sequence_ [copy recvBuf tgt empty [if d == 1 then 0 else 2*s | d <- b] | (b,recvBuf) <- zip bases recvBufs]

sendTo :: [Int] -> CVariable -> BuildM CVariable
sendTo b v = do
  let r = formatRank b
  req <- declScopedVariable Nothing (CRawType "MPI_Request") ("send_req_" ++ r) Nothing
  call "MPI_Isend" [ref v, "sizeof(" ++ variableName v ++ ")", "MPI_BYTE", "n->rank_" ++ r, "0", "n->mpi_world", ref req]
  return req

recvFrom :: [Int] -> CVariable -> BuildM CVariable
recvFrom b v = do
  let r = formatRank $ map negate b
  req <- declScopedVariable Nothing (CRawType "MPI_Request") ("recv_req_" ++ r) Nothing
  call "MPI_Irecv" [ref v, "sizeof(" ++ variableName v ++ ")", "MPI_BYTE", "n->rank_" ++ r, "0", "n->mpi_world", ref req]
  return req

wait :: CVariable -> BuildM ()
wait v = call "MPI_Wait" [ref v, "MPI_STATUS_IGNORE"]

call :: String -> [String] -> BuildM ()
call fn args = do
  stack <>= [Call fn args]
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

raw :: String -> BuildM ()
raw c = do
  stack <>= [Raw c]
  return ()

statement :: String -> BuildM ()
statement s = raw $ s ++ ";"
