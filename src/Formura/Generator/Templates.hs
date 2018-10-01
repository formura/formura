{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Formura.Generator.Templates where

import Control.Lens ((^.), view)
import Control.Monad.Writer
import Data.Foldable (toList, for_)
import Data.List
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, fromMaybe)
import Text.Printf

import Formura.NumericalConfig
import Formura.OrthotopeMachine.Graph
import Formura.GlobalEnvironment
import Formura.Generator.Types
import Formura.Generator.Functions
import Formura.Syntax

mapType :: TypeExpr -> CType
mapType (ElemType "Rational") = CDouble
mapType (ElemType "int") = CInt
mapType (ElemType "float") = CFloat
mapType (ElemType "double") = CDouble
mapType (ElemType x) = CRawType x
mapType (GridType _ x) = mapType x
mapType x = error $ "Unable to translate type to C:" ++ show x

scaffold :: GenM ()
scaffold = do
  ic <- view (omGlobalEnvironment . envNumericalConfig)
  axes <- view (omGlobalEnvironment . axesNames)
  typeName <- view (omGlobalEnvironment . gridStructTypeName)
  insntaceName <- view (omGlobalEnvironment . gridStructInstanceName)

  addHeader "<stdio.h>"
  addHeader "<stdlib.h>"
  addHeader "<stdbool.h>"
  addHeader "<math.h>"
  when (isJust $ ic ^. icMPIShape) $ addHeader "<mpi.h>"
  when (ic ^. icWithOmp /= 0) $ addHeader "<omp.h>"

  let gridPerNode = ic ^. icGridPerNode
  defineParam "Ns" (show $ ic ^. icSleeve)
  sequence_ [defineParam ("L" ++ show i) (show v) | (i,v) <- zip [1..] gridPerNode]
  case (ic ^. icMPIShape) of
    Nothing -> return ()
    Just mpiShape -> sequence_ [defineParam ("P" ++ show i) (show v) | (i,v) <- zip [1..] mpiShape]

  let mkFields :: M.Map IdentName TypeExpr -> [(String, CType)]
      mkFields = M.foldrWithKey (\k t acc -> (k, mapType t):acc) []
  gridStruct <- mkFields <$> view omStateSignature
  gridStructType <- defGlobalTypeStruct typeName gridStruct (SoA gridPerNode)

  globalData <- declGlobalVariable gridStructType insntaceName Nothing
  
  defUtilFunctions

  navi <- defGlobalFunction "Formura_Init" [(CPtr CInt, "argc"),(CRawType "char ***", "argv"),(CRawType "Formura_Navi *","n")] CVoid (\_ -> initBody "MPI_COMM_WORLD")
  defGlobalTypeStruct "Formura_Navi" navi Normal
  when (isJust $ ic ^. icMPIShape) $
    () <$ defGlobalFunction "Formura_Custom_Init" [(CRawType "Formura_Navi *","n"),(CRawType "MPI_Comm", "comm")] CVoid (\_ -> initBody "comm")
  defGlobalFunction "Formura_Finalize" [] CVoid (\_ -> finalizeBody)
  let forwardBody = case (ic ^. icBlockingType) of
                      NoBlocking -> (noBlocking gridStruct globalData)
                      TemporalBlocking gpb bpn nt -> temporalBlocking gridStruct globalData gpb bpn nt
  (buffType, rsltType) <- defGlobalFunction "Formura_Forward" [(CRawType "Formura_Navi *", "n")] CVoid (\_ -> forwardBody)
  dim <- view (omGlobalEnvironment . dimension)
  let blockOffset = [(CInt, "block_offset_" ++ show i) | i <- [1..dim]]
  defLocalFunction "Formura_Setup" ([(CRawType "Formura_Navi", "n")] ++ blockOffset) CVoid (setupBody globalData)
  defLocalFunction "Formura_Step" ([(CPtr buffType, "buff"), (CPtr rsltType, "rslt"),(CRawType "Formura_Navi", "n")] ++ blockOffset) CVoid stepBody

defUtilFunctions :: GenM ()
defUtilFunctions = do
  mmpiShape <- view (omGlobalEnvironment . envNumericalConfig . icMPIShape)
  case mmpiShape of
    Nothing -> return ()
    Just mpiShape -> do
      dim <- view (omGlobalEnvironment . dimension)
      let encodeRankArg = [(CInt, "p" ++ show i) | i <- [1..dim]]
      let decodeRankArg = (CInt,"p"):[(CPtr CInt, "p" ++ show i) | i <- [1..dim]]
      defLocalFunction "Formura_Encode_rank" encodeRankArg CInt $ \_ -> case dim of
        1 -> do
          let m1:_ = mpiShape
          statement $ printf "return (p1+%d)%%%d" m1 m1
        2 -> do
          let m1:m2:_ = mpiShape
          statement $ printf "return ((p1+%d)%%%d + %d*((p2+%d)%%%d))" m1 m1 m1 m2 m2
        3 -> do
          let m1:m2:m3:_ = mpiShape
          statement $ printf "return ((p1+%d)%%%d + %d*((p2+%d)%%%d) + %d*((p3+%d)%%%d))" m1 m1 m1 m2 m2 (m1*m2) m3 m3
        _ -> error "No support"
      defLocalFunction "Formura_Decode_rank" decodeRankArg CVoid $ \_ -> case dim of
        1 -> do
          let m1:_ = mpiShape
          statement $ printf "*p1 = (int)p%%%d" m1
        2 -> do
          let m1:_:_ = mpiShape
          statement $ printf "*p1 = (int)p%%%d" m1
          statement $ printf "*p2 = (int)p/%d" m1
        3 -> do
          let m1:m2:_:_ = mpiShape
          statement $ printf "int p4 = (int)p%%%d" (m1*m2)
          statement $ printf "*p1 = (int)p4%%%d" m1
          statement $ printf "*p2 = (int)p4/%d" m1
          statement $ printf "*p3 = (int)p/%d" (m1*m2)
        _ -> error "No support"

  axes <- view (omGlobalEnvironment . axesNames)
  gridPerNode <- view (omGlobalEnvironment . envNumericalConfig . icGridPerNode)
  sleeve <- view (omGlobalEnvironment . envNumericalConfig . icSleeve)
  let toPosBody a =
        statement $ printf "return n.space_interval_%s*((i+n.offset_%s)%%n.total_grid_%s)" a a a
  for_ axes $ \a ->
    defGlobalFunction ("to_pos_"++a) [(CInt, "i"), (CRawType "Formura_Navi", "n")] CDouble (\_ -> toPosBody a)

noBlocking :: [(String, CType)] -> CVariable -> BuildM GenM (CType, CType)
noBlocking gridStruct globalData = do
  s <- view (omGlobalEnvironment . envNumericalConfig . icSleeve)
  axes <- view (omGlobalEnvironment . axesNames)
  dim <- view (omGlobalEnvironment . dimension)
  gridPerNode <- view (omGlobalEnvironment . envNumericalConfig . icGridPerNode)
  buffType <- defLocalTypeStruct "Formura_Buff" gridStruct (SoA $ map (+ (2*s)) gridPerNode)
  rsltType <- defLocalTypeStruct "Formura_Rslt" gridStruct (SoA gridPerNode)
  buff <- declLocalVariable (Just "static") buffType "buff" Nothing
  rslt <- declLocalVariable (Just "static") rsltType "rslt" Nothing
  -- 通信
  sendrecv gridStruct globalData buff s

  copy globalData buff empty (repeat $ 2*s)
  -- 1ステップ更新
  call "Formura_Step" ([ref buff, ref rslt,"*n"] ++ (replicate dim "0"))
  for_ axes $ \a -> statement $ printf "n->offset_%s = (n->offset_%s - %d + n->total_grid_%s)%%n->total_grid_%s" a a s a a
  -- 結果の書き出し
  copy rslt globalData empty empty
  -- time_step を更新
  statement "n->time_step += 1"
  return (buffType, rsltType)

temporalBlocking :: [(String,CType)] -> CVariable -> [Int] -> [Int] -> Int -> BuildM GenM (CType, CType)
temporalBlocking gridStruct globalData gridPerBlock blockPerNode nt = do
  s <- view (omGlobalEnvironment . envNumericalConfig . icSleeve)
  axes <- view (omGlobalEnvironment . axesNames)
  dim <- view (omGlobalEnvironment . dimension)
  buffType <- defLocalTypeStruct "Formura_Buff" gridStruct (SoA $ map (+ (2*s)) gridPerBlock)
  rsltType <- defLocalTypeStruct "Formura_Rslt" gridStruct (SoA gridPerBlock)
  buff <- declLocalVariable (Just "static") buffType "buff" Nothing
  rslt <- declLocalVariable (Just "static") rsltType "rslt" Nothing
  -- 床の準備
  tmpFloorType <- defLocalTypeStruct "Formura_Tmp_Floor" gridStruct (SoA $ zipWith (*) gridPerBlock blockPerNode)
  tmpFloor <- declLocalVariable (Just "static") tmpFloorType "tmp_floor" Nothing
  copy globalData tmpFloor empty (repeat (2*s*nt))
  -- 壁の準備
  -- 3次元の場合の壁の大きさ
  -- x壁: [MY][MZ][NT][2*Ns][NY+2*Ns][NZ+2*Ns]
  -- y壁: [MX][MZ][NT][NX+2*Ns][2*Ns][NZ+2*Ns]
  -- z壁: [MX][MY][NT][NX+2*Ns][NY+2*Ns][2*Ns]
  tmpWalls <- forM [([i == j | j <- [1..dim]],i) | i <- [1..dim]] $ \(flag,i) -> do
    let bs = [n | (n,b) <- zip blockPerNode flag, not b]
        gs = [if b then 2*s else n+2*s | (n,b) <- zip gridPerBlock flag]
    tmpWallType <- defLocalTypeStruct ("Formura_Tmp_Wall_" ++ show i) gridStruct (SoA $ bs ++ [nt] ++ gs)
    tmpWall <- declLocalVariable (Just "static") tmpWallType ("tmp_wall_" ++ show i) Nothing
    return (flag, gs, tmpWall)
  -- 通信
  rs <- isendrecv gridStruct globalData (s*nt)
  -- 即時計算可能なブロックと通信待ちブロックの分離
  let b0 = [(0,m-1-d) | (n,m) <- zip gridPerBlock blockPerNode, let d = 2*s*nt `div` n]
      bs = [[(if i == j then m-1-d else 0, if i > j then m-1-d else m) | (n,m,i) <- zip3 gridPerBlock blockPerNode [1..dim], let d = 2*s*nt `div` n] | j <- [1..dim]]
  -- 床の更新 (ブロックごとに)
  let update boundary = loopWith [("j" ++ show i,l,u,1) | (i,(l,u)) <- zip [1..dim] boundary] $ \idx -> do
      -- - 床の読み込み
        let floorOffset = toIdx ["+" ++ show (n*(m-1)) ++ "-" ++ show n ++ "*" | (n,m) <- zip gridPerBlock blockPerNode] <> idx
        copy tmpFloor rslt floorOffset empty
      -- - NT段更新
        loopWith [("it",0,nt,1)] $ \it -> do
      --   - buffに床をセット
          copy rslt buff empty empty
      --   - buffに壁をセット
          for_ tmpWalls $ \(flag, gs, tmpWall) -> do
            let idx0 = (toIdx [i | (i,b) <- zip (fromIdx idx) flag, not b]) >< it
            loop gs $ \idx' ->
              tell [mkIdent f buff (idx' <> toIdx [if b then n else 0 | (b,n) <- zip flag gridPerBlock]) @= mkIdent f tmpWall (idx0 >< idx') | f <- (getFields tmpWall), f `elem` (getFields buff)]
            return ()
      --   - 1段更新
          call "Formura_Step" ([ref buff, ref rslt,"*n"] ++ fromIdx floorOffset)
          for_ axes $ \a -> statement $ printf "n->offset_%s = (n->offset_%s - %d + n->total_grid_%s)%%n->total_grid_%s" a a s a a
      --   - 壁の書き出し
          for_ tmpWalls $ \(flag, gs, tmpWall) -> do
            let idx0 = (toIdx [i | (i,b) <- zip (fromIdx idx) flag, not b]) >< it
            loop gs $ \idx' ->
              tell [mkIdent f tmpWall (idx0 >< idx')  @= mkIdent f buff idx' | f <- (getFields buff), f `elem` (getFields tmpWall)]
            return ()
      -- - 床の書き出し
        copy rslt tmpFloor empty floorOffset

  update b0
  waitAndCopy rs tmpFloor (s*nt)
  mapM_ update bs
  copy tmpFloor globalData empty empty
  statement $ "n->time_step += " ++ show nt
  return (buffType, rsltType)

setupMPI :: [Int] -> String -> BuildM GenM [(String, CType)]
setupMPI mpiShape comm = do
  let p0 = product mpiShape
  dim <- view (omGlobalEnvironment . dimension)
  when (comm == "MPI_COMM_WORLD") $ call "MPI_Init" ["argc", "argv"]
  declLocalVariable Nothing (CRawType "MPI_Comm") "cm" (Just comm)
  statement "int size, rank"
  statement "MPI_Comm_size(cm, &size)"
  statement "MPI_Comm_rank(cm, &rank)"
  raw $ printf "if(size != %d) {\nfprintf(stderr,\"Do not match the number of MPI process!\");\nexit(1);\n}" p0
  bases <- view (omGlobalEnvironment . commBases)
  statement $ "int " ++ intercalate "," ["i" ++ show i | i <- [1..dim]]
  call "Formura_Decode_rank" ("rank":["&i"++show i | i <- [1..dim]])
  let rs = bases ++ [map negate b | b <- bases]
      r2a x | x == 0 = ""
            | x == 1 = "+1"
            | x == -1 = "-1"
            | otherwise = error "Error in r2a"
      rank2arg r = "(" ++ intercalate "," ["i" ++ show i ++ r2a x | (i,x) <- zip [1..length r] r] ++ ")"
      ranksTable = [(formatRank r,rank2arg r) | r <- rs ]
  let set n t v = statement ("n->" ++ n ++ " = " ++ v) >> return (n, t)
  myRank <- set "my_rank" CInt "rank"
  mpiWorld <- set "mpi_world" (CRawType "MPI_Comm") "cm"
  ranks <- mapM (\(r,ag) -> set ("rank_" ++ r) CInt ("Formura_Encode_rank" ++ ag)) ranksTable
  return $ [myRank,mpiWorld] <> ranks

initBody :: String -> BuildM GenM [(String,CType)]
initBody comm = do
  axes <- view (omGlobalEnvironment . axesNames)
  gridPerNode <- view (omGlobalEnvironment . envNumericalConfig . icGridPerNode)
  lengthPerNode <- view (omGlobalEnvironment . envNumericalConfig . icLengthPerNode)
  dim <- view (omGlobalEnvironment . dimension)
  mmpiShape <- view (omGlobalEnvironment . envNumericalConfig . icMPIShape)
  spaceIntervals <- view (omGlobalEnvironment . envNumericalConfig . icSpaceInterval)
  let totalGrid = zipWith (*) gridPerNode (fromMaybe (repeat 1) mmpiShape)
  let set n t v = statement ("n->" ++ n ++ " = " ++ v) >> return (n, t)
  fs <- case mmpiShape of
          Nothing -> do
            myRank <- set "my_rank" CInt "0"
            offsets <- mapM (\a -> set ("offset_" ++ a) CInt "0") axes
            lengthes <- mapM (\(a,v) -> set ("length_" ++ a) CDouble (show v)) $ zip axes lengthPerNode
            return $ [myRank] <> offsets <> lengthes
          Just mpiShape -> do
            fs' <- setupMPI mpiShape comm
            offsets <- mapM (\(a,i,l) -> set ("offset_" ++ a) CInt (show l++"*i"++show i)) $ zip3 axes [1..dim] gridPerNode
            lengthes <- mapM (\(a,v) -> set ("length_" ++ a) CDouble (show v)) $ zip axes $ zipWith (\l m -> l * fromIntegral m) lengthPerNode mpiShape
            return $  fs' <> offsets <> lengthes
  timeStep <- set "time_step" CInt "0"
  lowers <- mapM (\a -> set ("lower_" ++ a) CInt "0") axes
  uppers <- mapM (\(a,v) -> set ("upper_" ++ a) CInt (show v)) $ zip axes gridPerNode
  totals <- mapM (\(a,v) -> set ("total_grid_" ++ a) CInt (show v)) $ zip axes totalGrid
  intervals <- mapM (\(a,v) -> set ("space_interval_" ++ a) CDouble (show v)) $ zip axes spaceIntervals
  call "Formura_Setup" ("*n":replicate dim "0")
  let navi = [timeStep] <> lowers <> uppers <> totals <> intervals <> fs
  return navi

finalizeBody :: BuildM GenM ()
finalizeBody = do
  mmpiShape <- view (omGlobalEnvironment . envNumericalConfig . icMPIShape)
  case mmpiShape of
    Nothing -> return ()
    Just _ -> call "MPI_Finalize" []

setupBody :: CVariable -> [CVariable] -> BuildM GenM ()
setupBody globalData _ = do
  mmg <- view omInitGraph
  mkKernel mmg [globalData,globalData]

stepBody :: [CVariable] -> BuildM GenM ()
stepBody args = do
  mmg <- view omStepGraph
  mkKernel mmg args

-- Manifestノードの配列サイズを計算する
-- もとの配列サイズ NX+2Ns に比べてどれだけ小さいかを求める
calcSizes :: MMGraph -> M.Map OMNodeID Int
calcSizes = M.foldlWithKey (\acc k (Node mi _ _) -> M.insert k (worker mi acc) acc) M.empty
  where
    worker :: MMInstruction -> M.Map OMNodeID Int -> Int
    worker mi tbl = maximum' $ M.foldr go [] mi
      where go (Node (LoadCursorStatic s _) _ _) acc = (maximum $ abs s):acc
            go (Node (LoadCursor s oid) _ _) acc = let s' = maximum $ abs s
                                                       n0 = tbl M.! oid
                                                    in  (n0+s'):acc
            go _ acc = acc

            maximum' [] = 0
            maximum' x = maximum x

mkKernel :: MMGraph -> [CVariable] -> BuildM GenM ()
mkKernel mmg args = do
  let inputSize = getSize $ variableType $ args !! 0
  let sizeTable = calcSizes mmg
  axes <- view (omGlobalEnvironment . axesNames)
  -- 中間変数を生成するかどうかを判定する
  -- 型が void なら最後に Store されているはずなので、中間変数を生成しない
  -- そうでないなら、最後に型に合う変数に結果を書き込む必要がある (最後に命令として Store がない)
  let isVoid (ElemType "void") = True
      isVoid _ = False

      tmpPrefix = "formura_mn"
      tmpName oid = tmpPrefix ++ show oid

      insertMNStore :: OMNodeID -> MMInstruction -> MMInstruction
      insertMNStore oid mm = M.insert (MMNodeID mmid) node mm
        where mmid = M.size mm
              node = Node (Store (tmpName oid) (MMNodeID $ mmid-1)) (ElemType "void") []
  let (tmps, mmg') = M.foldrWithKey (\omid (Node mm omt x) (ts,g) -> if isVoid omt then (ts,M.insert omid (Node mm omt x) g) else ((omid,omt):ts,M.insert omid (Node (insertMNStore omid mm) omt x) g)) ([],M.empty) mmg
  -- 中間変数の宣言
  let mapTmpType :: Int -> OMNodeType -> CType
      mapTmpType _ (ElemType "Rational") = CDouble
      mapTmpType _ (ElemType "void") = CVoid
      mapTmpType _ (ElemType "double") = CDouble
      mapTmpType _ (ElemType "float") = CFloat
      mapTmpType _ (ElemType "int") = CInt
      mapTmpType _ (ElemType x) = CRawType x
      mapTmpType ds (GridType _ x) = CArray [s-2*ds | s <- inputSize] (mapTmpType ds x)
      mapTmpType _ _ = error "Invalid type"
  sequence_ [declLocalVariable (Just "static") (mapTmpType (sizeTable M.! omid) omt) (tmpName omid) Nothing | (omid,omt) <- tmps]
  -- 命令の変換
  let formatNode i = "a" ++ show i
      mapType' :: MicroNodeType -> CType
      mapType' (ElemType "Rational") = CDouble
      mapType' (ElemType "void") = CVoid
      mapType' (ElemType "double") = CDouble
      mapType' (ElemType "float") = CFloat
      mapType' (ElemType "int") = CInt
      mapType' (ElemType x) = CRawType x
      mapType' _ = error "Invalid type"
  let genMicroInst idx _ _ (Store n x) _ | tmpPrefix `isPrefixOf` n = tell [(n ++ show idx) @= formatNode x]
                                         | otherwise = tell [(mkIdent n (args !! 1) idx) @= formatNode x]
      genMicroInst idx s mmid mi mt =
        let decl x = declLocalVariable Nothing (mapType' mt) (formatNode mmid) (Just x) >> return ()
        in decl $ case mi of
            (LoadCursorStatic d n) -> mkIdent n (args !! 0) (idx <> (toIdx . toList $ d + pure s))
            (LoadCursor d oid) -> tmpName oid ++ show (idx <> (toIdx . toList $ d + pure (s-(sizeTable M.! oid))))
            (Imm r) -> show (realToFrac r :: Double)
            (Uniop op a) | "external-call" `isPrefixOf` op -> (fromJust $ stripPrefix "external-call/" op) ++ "(" ++ formatNode a ++ ")"
                         | otherwise -> op ++ formatNode a
            (Binop op a b) | op == "**" -> "pow(" ++ formatNode a ++ "," ++ formatNode b ++ ")"
                           | otherwise -> formatNode a ++ op ++ formatNode b
            (Triop _ a b c) -> formatNode a ++ "?" ++ formatNode b ++ ":" ++ formatNode c
            -- LoadIndex をサポートするにはグローバルな配列に対するオフセットが必要
            -- つまり、Formura_Step のAPIを変更する必要がある
            LoadIndex i -> (fromIdx idx) !! i ++ "+n.offset_" ++ (axes !! i) ++ "+block_offset_" ++ show (i+1)
            -- Naryop は廃止かもなので、実装を待つ
            -- Naryop op xs -> undefined
            x -> error $ "Unimplemented for keyword: " ++ show x
  let genMMInst :: Int -> MMInstruction -> BuildM GenM ()
      genMMInst s mm = loop [s'-2*s | s' <- inputSize] $ \idx -> sequence_ [genMicroInst idx s mmid mi mt | (mmid, Node mi mt _) <- M.toAscList mm]
  sequence_ [genMMInst (sizeTable M.! omid) mm | (omid, Node mm _ _) <- M.toAscList mmg']
