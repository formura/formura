{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Formura.Generator.Templates where

import Control.Lens ((^.), view)
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List
import qualified Data.Map as M
import Data.Monoid
import Text.Printf

import Formura.NumericalConfig
import Formura.OrthotopeMachine.Graph
import Formura.GlobalEnvironment
import Formura.Generator.Types
import Formura.Generator.Functions
import Formura.Syntax
import Formura.Vec

scaffold :: GenM ()
scaffold = do
  dim <- view (omGlobalEnvironment . dimension)
  ic <- view (omGlobalEnvironment . envNumericalConfig)
  axes <- view (omGlobalEnvironment . axesNames)
  stepGraph <- view omStepGraph

  addHeader "<stdio.h>"
  addHeader "<stdbool.h>"
  addHeader "<math.h>"
  addHeader "<mpi.h>"

  let gridPerNode = ic ^. icGridPerNode
  let mkFields :: M.Map IdentName TypeExpr -> [(String, CType)]
      mkFields = M.foldrWithKey (\k t acc -> (k, mapType t):acc) []
        where mapType (ElemType "Rational") = CDouble
              mapType (ElemType "int") = CInt
              mapType (ElemType "float") = CFloat
              mapType (ElemType "double") = CDouble
              mapType (ElemType x) = CRawType x
              mapType (GridType _ x) = mapType x
              mapType x = error $ "Unable to translate type to C:" ++ show x
  gridStruct <- mkFields <$> view omStateSignature
  gridStructType <- defGlobalTypeStruct "Formura_Grid_Struct" gridStruct (SoA gridPerNode)

  let spaceIntervals = ic ^. icSpaceInterval
  sequence_ [declGlobalVariable CDouble ("d" ++ a) (Just $ show d) | (a,d) <- zip axes spaceIntervals]
  globalData <- declGlobalVariable gridStructType "formura_data" Nothing
  
  defGlobalFunction "to_pos_x" [] CDouble (return ())

  defGlobalFunction "Formura_Init" [(CRawType "Formura_Navi *","n")] CVoid initBody
  let forwardBody = case (ic ^. icBlockingType) of
                      NoBlocking -> (noBlocking gridStruct globalData)
  (buffType, rsltType) <- defGlobalFunction "Formura_Forward" [(CRawType "Formura_Navi *", "n")] CVoid forwardBody
  let stepBody = raw $ mkStep axes stepGraph
  defLocalFunction "Formura_Step" [(buffType, "buff"), (rsltType, "rslt")] CVoid stepBody

initBody :: BuildM GenM ()
initBody = do
  raw "int size, rank"
  raw "MPI_Comm_size(comm, &size)"
  raw "MPI_Comm_rank(comm, &rank)"
  -- TODO: Formura_Decode_rank 関数の呼び出し
  axes <- view (omGlobalEnvironment . axesNames)
  gridPerNode <- view (omGlobalEnvironment . envNumericalConfig . icGridPerNode)
  lengthPerNode <- view (omGlobalEnvironment . envNumericalConfig . icLengthPerNode)
  dim <- view (omGlobalEnvironment . dimension)
  let rankConv x | x == 0 = ("0","")
                 | x == 1 = ("p1","+1")
                 | x == -1 = ("m1","-1")
  let ranksTable | dim == 1 = [ (r,"(i1" ++ a ++ ")") | x <- [1,-1], let (r,a) = rankConv x]
                 | dim == 2 = [(r1 ++ "_" ++ r2, "(i1"++a1 ++ ",i2" ++ a2 ++ ")") | (x,y) <- [(1,0), (-1,0), (0,1), (0,-1), (1,1), (-1,-1)],let (r1,a1) = rankConv x, let (r2,a2) = rankConv y]
                 | dim == 3 = [(r1 ++ "_" ++ r2 ++ "_" ++ r3, "(i1"++a1++",i2"++a2++",i3"++a3++")") | (x,y,z) <- [(1,0,0), (-1,0,0), (0,1,0), (0,-1,0), (0,0,1), (0,0,-1), (1,1,0), (-1,-1,0), (1,0,1), (-1,0,-1), (0,1,1), (0,-1,-1), (1,1,1), (-1,-1,-1)], let (r1,a1) = rankConv x, let (r2,a2) = rankConv y, let (r3,a3) = rankConv z]
                 | otherwise = error "Not support"
  let set n t v = raw ("n->" ++ n ++ " = " ++ v) >> return (n, t)
  myRank <- set "my_rank" CInt "0"
  timeStep <- set "time_step" CInt "0"
  mpiWorld <- set "mpi_world" (CRawType "MPI_Comm") "comm"
  lowers <- mapM (\a -> set ("lower_" ++ a) CInt "0") axes
  uppers <- mapM (\(a,v) -> set ("upper_" ++ a) CInt (show v)) $ zip axes gridPerNode
  offsets <- mapM (\a -> set ("offset_" ++ a) CInt "0") axes
  lengthes <- mapM (\(a,v) -> set ("length_" ++ a) CDouble (show v)) $ zip axes lengthPerNode
  ranks <- mapM (\(r,ag) -> set ("rank_" ++ r) CInt ("Formura_Encode_rank" ++ ag)) ranksTable
  let navi = [myRank, timeStep, mpiWorld] <> lowers <> uppers <> offsets <> lengthes <> ranks
  defGlobalTypeStruct "Formura_Navi" navi Normal
  return ()


noBlocking :: [(String, CType)] -> CVariable -> BuildM GenM (CType, CType)
noBlocking gridStruct globalData = do
  s <- view (omGlobalEnvironment . envNumericalConfig . icSleeve)
  gridPerNode <- view (omGlobalEnvironment . envNumericalConfig . icGridPerNode)
  buffType <- defLocalTypeStruct "Formura_Buff" gridStruct (SoA $ map (+ (2*s)) gridPerNode)
  rsltType <- defLocalTypeStruct "Formura_Rslt" gridStruct (SoA gridPerNode)
  buff <- declLocalVariable (Just "static") buffType "buff" Nothing
  rslt <- declLocalVariable (Just "static") rsltType "rslt" Nothing
  -- 通信
  recvData <- sendrecv globalData s
  copy recvData buff [] []
  copy globalData buff [] []
  -- 1ステップ更新
  call "Formura_Step" [ref buff, ref rslt]
  -- 結果の書き出し
  copy rslt globalData [] []
  -- time_step を更新
  raw "n->time_step += 1"
  return (buffType, rsltType)

-- Manifestノードの配列サイズを計算する
-- もとの配列サイズ NX+2Ns に比べてどれだけ小さいかを求める
calcSizes :: MMGraph -> M.Map OMNodeID Int
calcSizes = M.foldlWithKey (\acc k (Node mi _ _) -> M.insert k (worker mi acc) acc) M.empty
  where
    worker :: MMInstruction -> M.Map OMNodeID Int -> Int
    worker mi tbl = maximum $ M.foldr go [] mi
      where go (Node (LoadCursorStatic s _) _ _) acc = (maximum $ abs s):acc
            go (Node (LoadCursor s oid) _ _) acc = let s' = maximum $ abs s
                                                       n0 = tbl M.! oid
                                                    in  (n0+s'):acc
            go _ acc = acc

mkStep :: [String] -> MMGraph -> String
mkStep axes mmg = withTmp $ unlines [ genMMInst omid $ if isVoid omt then mm else insertMNStore omid mm | (omid, (Node mm omt _)) <- M.toAscList mmg ]
  where
    sizeTable = calcSizes mmg
    -- 中間変数を生成するかどうかを判定する
    -- 型が void なら最後に Store されているはずなので、中間変数を生成しない
    -- そうでないなら、最後に型に合う変数に結果を書き込む必要がある (最後に命令として Store がない)
    isVoid (ElemType "void") = True
    isVoid _ = False

    insertMNStore :: OMNodeID -> MMInstruction -> MMInstruction
    insertMNStore oid mm = M.insert (MMNodeID mmid) node mm
      where mmid = M.size mm
            node = Node (Store (formatTmp oid) (MMNodeID $ mmid-1)) (ElemType "void") []

    withTmp body = unlines [ genTmpDecl omt omid ++ ";" | (omid, (Node _ omt _)) <- M.toAscList mmg, not (isVoid omt) ] ++ body

    genTmpDecl (ElemType "Rational") oid = "double " ++ formatTmp oid
    genTmpDecl (ElemType x) oid = x ++ " " ++ formatTmp oid
    genTmpDecl (GridType _ x) oid = let s = sizeTable M.! oid
                                    in (genTmpDecl x oid) ++ formatSize s
    genTmpDecl x _ = error $ "Unable to translate type to C:" ++ show x

    genMMInst :: OMNodeID -> MMInstruction -> String
    genMMInst omid mm = inLoop s $ unlines [ genMicroInst mmid mi mt s ++ ";" | (mmid, (Node mi mt _)) <- M.toAscList mm ]
      where s = sizeTable M.! omid

    inLoop s body = foldr (\a acc -> let a' = map toLower a in printf "for(int i%s = 0; i%s < N%s + 2*(Ns - %d); i%s++) {\n%s}\n" a' a' a s a' acc) body axes

    formatType :: MicroNodeType -> String
    formatType (ElemType "Rational") = "double "
    formatType (ElemType "void") = ""
    formatType (ElemType x) = x ++ " "
    formatType _ = error "Invalid type"

    formatIndex :: Vec Int -> String
    formatIndex di = concat [printf "[i%s%+d]" (map toLower a) i | (a,i) <- zip axes $ toList di]

    formatNode :: MMNodeID -> String
    formatNode i = "a" ++ show i

    formatTmp :: OMNodeID -> String
    formatTmp i = "MN" ++ show i

    formatSize :: Int -> String
    formatSize s = concat [printf "[N%s+2*(Ns-%d)]" a s | a <- axes]

    genMicroInst _ (Store name x) _ _ | "MN" `isPrefixOf` name = name ++ formatIndex (Vec [0,0,0]) ++ " = " ++ formatNode x
                                      | otherwise = "rslt->" ++ name ++ formatIndex (Vec [0,0,0]) ++ " = " ++ formatNode x
    genMicroInst mmid mi mt s =
      let lhs = formatType mt ++ formatNode mmid
          rhs = case mi of
                  LoadCursorStatic d name -> "buff->" ++ name ++ formatIndex (d + pure s)
                  LoadCursor d nid -> let s' = sizeTable M.! nid in formatTmp nid ++ formatIndex (d + pure (s-s'))
                  Imm r -> show (realToFrac r :: Double)
                  Uniop op a | "external-call/" `isPrefixOf` op -> let Just f = stripPrefix "extern-call/" op in f ++ "(" ++ formatNode a ++ ")"
                             | otherwise -> op ++ formatNode a
                  Binop op a b | op == "**" -> "pow(" ++ formatNode a ++ "," ++ formatNode b ++ ")"
                               | otherwise -> formatNode a ++ op ++ formatNode b
                  Triop "ite" a b c -> formatNode a ++ "?" ++ formatNode b ++ ":" ++ formatNode c
                  -- LoadIndex をサポートするにはグローバルな配列に対するオフセットが必要
                  -- つまり、Formura_Step のAPIを変更する必要がある
                  -- LoadIndex i -> "i" ++ (map toLower $ axes !! i)
                  -- Naryop は廃止かもなので、実装を待つ
                  -- Naryop op xs -> undefined
                  x -> error $ "Unimplemented for keyword: " ++ show x
      in unwords [lhs, "=", rhs]

