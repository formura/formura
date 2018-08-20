{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
module Formura.Generator.Templates where

import Control.Lens ((^.), view)
import qualified Data.Map as M
import Data.Monoid
import Data.Scientific

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

  let lengthPerNode = ic ^. icLengthPerNode
  let (Vec spaceIntervals) = (fmap (toRealFloat @Double) lengthPerNode) / (fmap fromIntegral gridPerNode)
  sequence_ [declGlobalVariable CDouble ("d" ++ a) (Just $ show d) | (a,d) <- zip axes spaceIntervals]
  globalData <- declGlobalVariable gridStructType "formura_data" Nothing
  
  defGlobalFunction "to_pos_x" [] CDouble (return ())

  defGlobalFunction "Formura_Init" [(CRawType "Formura_Navi *","n")] CVoid initBody
  -- let forwardBody = case (ic ^. icBlockingType) of
  --                     NoBlocking -> (noBlocking gridStruct globalData)
  let forwardBody = noBlocking gridStruct globalData
  (buffType, rsltType) <- defGlobalFunction "Formura_Forward" [(CRawType "Formura_Navi *", "n")] CVoid forwardBody
  let stepBody = undefined
  defLocalFunction "Formura_Step" [(buffType, "buff"), (rsltType, "rslt")] CVoid stepBody

initBody :: BuildM GenM ()
initBody = do
  raw "int size, rank"
  raw "MPI_Comm_size(comm, &size)"
  raw "MPI_Comm_rank(comm, &rank)"
  -- TODO: Formura_Decode_rank 関数の呼び出し
  axes <- view (omGlobalEnvironment . axesNames)
  (Vec gridPerNode) <- view (omGlobalEnvironment . envNumericalConfig . icGridPerNode)
  (Vec lengthPerNode) <- view (omGlobalEnvironment . envNumericalConfig . icLengthPerNode)
  dim <- view (omGlobalEnvironment . dimension)
  -- こんな感じで人力でやるのもバカらしいので改善する
  let ranksTable = undefined
  -- let ranksTable | dim == 1 = [ ("p1", "(i1+1)"), ("m1","(i1-1)") ]
  --                | dim == 2 = [ ("p1_0","(i1+1,i2)"), ("m1_0","(i1-1,i2)"), ("0_p1","(i1,i2+1)"), "0_m1", "p1_p1", "m1_m1"]
  --                | dim == 3 = [ "p1_0_0", "m1_0_0"
  --                             , "0_p1_0", "0_m1_0"
  --                             , "0_0_p1", "0_0_m1"
  --                             , "p1_p1_0", "m1_m1_0"
  --                             , "p1_0_p1", "m1_0_m1"
  --                             , "0_p1_p1", "0_m1_m1"
  --                             , "p1_p1_p1", "m1_m1_m1"
  --                             ]
  --                | otherwise = error "Not support"
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
  buffType <- defLocalTypeStruct "Formura_Buff" gridStruct (SoA $ gridPerNode + pure (2*s))
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

