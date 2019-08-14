{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
module Formura.Generator.Templates where

import           Control.Lens hiding (op)
import           Control.Monad
import           Data.Foldable (for_, toList)
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Text.Printf

import Formura.Generator.Functions
import Formura.Generator.Types
import Formura.GlobalEnvironment
import Formura.IR
import Formura.NumericalConfig
import Formura.OrthotopeMachine.Graph
import Formura.Syntax

scaffold :: BuildM ()
scaffold = do
  addHeader "<stdio.h>"
  addHeader "<stdlib.h>"
  addHeader "<stdbool.h>"
  addHeader "<math.h>"
  withMPI $ \_ -> addHeader "<mpi.h>"
  withOMP $ addHeader "<omp.h>"

  ic <- view (globalEnvironment . envNumericalConfig)
  let gridPerNode = ic ^. icGridPerNode
  defineParam "Ns" (show $ ic ^. icSleeve)
  sequence_ [defineParam ("L" ++ show @Int i) (show v) | (i,v) <- zip [1..] gridPerNode]
  withMPI $ \mpiShape -> sequence_ [defineParam ("P" ++ show @Int i) (show v) | (i,v) <- zip [1..] mpiShape]

  defGlobalData
  navi <- defNavi
  defLocalBuffs (ic ^. icBlockingType)
  defCommBuffs

  defUtilFunctions

  defFormuraSetup
  withFirstStep defFormuraFirstStep
  withFilter defFormuraFilter
  defFormuraInit navi
  defFormuraStep
  defFormuraForward
  defFormuraFinalize

defGlobalData :: BuildM ()
defGlobalData = do
  gridPerNode <- view (globalEnvironment . envNumericalConfig . icGridPerNode)
  typeName <- view (globalEnvironment . gridStructTypeName)
  insntaceName <- view (globalEnvironment . gridStructInstanceName)
  gridStruct <- mkFields <$> view irStateSignature
  target .= gridStruct
  gridStructType <- defGlobalTypeStruct typeName gridStruct (SoA gridPerNode)
  () <$ declGlobalVariable gridStructType insntaceName Nothing
  where
    mapType :: TypeExpr -> CType
    mapType (ElemType "Rational") = CDouble
    mapType (ElemType "int") = CInt
    mapType (ElemType "float") = CFloat
    mapType (ElemType "double") = CDouble
    mapType (ElemType x) = CRawType x
    mapType (GridType _ x) = mapType x
    mapType x = error $ "Unable to translate type to C:" ++ show x

    mkFields :: M.Map IdentName TypeExpr -> [(String, CType)]
    mkFields = M.foldrWithKey (\k t acc -> (k, mapType t):acc) []

defNavi :: BuildM [(String,String)]
defNavi = do
  navi <- mkNavi
  defGlobalTypeStruct "Formura_Navi" [(n,t) | (n,t,_) <- navi] Normal
  return [(n,v) | (n,_,v) <- navi]

mkNavi :: BuildM [(String,CType,String)]
mkNavi = do
  dim <- view (globalEnvironment . dimension)
  axes <- view (globalEnvironment . axesNames)
  gridPerNode <- view (globalEnvironment . envNumericalConfig . icGridPerNode)
  lengthPerNode <- view (globalEnvironment . envNumericalConfig . icLengthPerNode)
  spaceIntervals <- view (globalEnvironment . envNumericalConfig . icSpaceInterval)
  mmpiShape <- view (globalEnvironment . envNumericalConfig . icMPIShape)
  fs <- case mmpiShape of
          Just mpiShape -> do
            bases <- view (globalEnvironment . commBases)
            let rs = bases ++ [map negate b | b <- bases]
                ranksTable = [(formatRank r,rank2arg r) | r <- rs ]
            return $ [ ("my_rank",CInt,"rank")
                     , ("mpi_world", CRawType "MPI_Comm", "cm")]
                  ++ [("rank_"++r,CInt,"Formura_Encode_rank"++ag) | (r,ag) <- ranksTable]
                  ++ [("offset_"++a,CInt,show l++"*i"++show i) | (a,l,i) <- zip3 axes gridPerNode [1..dim]]
                  ++ [("length_"++a,CDouble,show (l*fromIntegral m)) | (a,l,m) <- zip3 axes lengthPerNode mpiShape]
                  ++ [("total_grid_"++a,CInt,show (l*m)) | (a,l,m) <- zip3 axes gridPerNode mpiShape]
          Nothing ->
            return $ [("my_rank",CInt,"0")]
                  ++ [("offset_"++a,CInt,"0") | a <- axes]
                  ++ [("length_"++a,CDouble,show l) | (a,l) <- zip axes lengthPerNode]
                  ++ [("total_grid_"++a,CInt,show l) | (a,l) <- zip axes gridPerNode]
  return $ [("time_step",CInt,"0")]
        ++ [("lower_"++a,CInt,"0") | a <- axes]
        ++ [("upper_"++a,CInt,show l) | (a,l) <- zip axes gridPerNode]
        ++ [("space_interval_"++a,CDouble,show l) | (a,l) <- zip axes spaceIntervals]
        ++ fs
  where
    r2a x | x == 0 = ""
          | x == 1 = "+1"
          | x == -1 = "-1"
          | otherwise = error "Error in r2a"
    rank2arg r = "(" ++ intercalate "," ["i" ++ show i ++ r2a x | (i,x) <- zip [1..length r] r] ++ ")"

-- NoBlocking:
--   Buff
--   Rslt
-- Temporal Blocking:
--   Tmp_Floor
--   Tmp_Walls
--   Buff
--   Rslt
defLocalBuffs :: BlockingType -> BuildM ()
defLocalBuffs NoBlocking = do
  gridStruct <- use target
  s <- maximum <$> getSleeves
  gridPerNode <- view (globalEnvironment . envNumericalConfig . icGridPerNode)
  buffType <- defLocalTypeStruct "Formura_Buff" gridStruct (SoA $ map (+ (2*s)) gridPerNode)
  buff <- declLocalVariable (Just "static") buffType "buff" Nothing
  globalData <- getGlobalData
  addVariable "step_input" buff
  addVariable "step_output" globalData
  withFirstStep $ \_ -> do
    addVariable "first_input" buff
    addVariable "first_output" globalData
  withFilter $ \_ -> do
    addVariable "filter_input" buff
    addVariable "filter_output" globalData
defLocalBuffs (TemporalBlocking gridPerBlock blockPerNode nt) = do
  s <- view (globalEnvironment . envNumericalConfig . icSleeve)
  dim <- view (globalEnvironment . dimension)
  gridStruct <- use target
  buffType <- defLocalTypeStruct "Formura_Buff" gridStruct (SoA $ map (+ (2*s)) gridPerBlock)
  rsltType <- defLocalTypeStruct "Formura_Rslt" gridStruct (SoA gridPerBlock)
  buff <- declLocalVariable (Just "static") buffType "buff" Nothing
  rslt <- declLocalVariable (Just "static") rsltType "rslt" Nothing
  -- 床の準備
  ms <- maximum <$> getSleeves
  gridPerNode <- view (globalEnvironment . envNumericalConfig . icGridPerNode)
  tmpFloorType <- defLocalTypeStruct "Formura_Tmp_Floor" gridStruct (SoA $ map (+ (2*ms)) gridPerNode)
  tmpFloor <- declLocalVariable (Just "static") tmpFloorType "tmp_floor" Nothing
  -- 壁の準備
  -- 3次元の場合の壁の大きさ
  -- x壁: [MY][MZ][NT][2*Ns][NY+2*Ns][NZ+2*Ns]
  -- y壁: [MX][MZ][NT][NX+2*Ns][2*Ns][NZ+2*Ns]
  -- z壁: [MX][MY][NT][NX+2*Ns][NY+2*Ns][2*Ns]
  for_ [([i == j | j <- [1..dim]],i) | i <- [1..dim]] $ \(flag,i) -> do
    let bs = [n | (n,b) <- zip blockPerNode flag, not b]
        gs = [if b then 2*s else n+2*s | (n,b) <- zip gridPerBlock flag]
    tmpWallType <- defLocalTypeStruct ("Formura_Tmp_Wall_" ++ show i) gridStruct (SoA $ bs ++ [nt] ++ gs)
    declLocalVariable (Just "static") tmpWallType ("tmp_wall_" ++ show i) Nothing
  globalData <- getGlobalData
  addVariable "step_input" buff
  addVariable "step_output" rslt
  withFirstStep $ \_ -> do
    addVariable "first_input" tmpFloor
    addVariable "first_output" globalData
  withFilter $ \_ -> do
    addVariable "filter_input" tmpFloor
    addVariable "filter_output" globalData

defCommBuffs :: BuildM ()
defCommBuffs = do
  bases <- view (globalEnvironment . commBases)
  gridPerNode <- view (globalEnvironment . envNumericalConfig . icGridPerNode)
  gridStruct <- use target
  commType <- defLocalTypeStruct "Formura_Comm_Buff" gridStruct Normal
  sleeves <- getSleeves
  for_ sleeves $ \s ->
    for_ bases $ \b -> do
      let r = formatRank b
          r' = formatRank $ map negate b
      declLocalVariable (Just "static") (CArray [if d == 1 then 2*s else n | (d,n) <- zip b gridPerNode] commType) ("send_buf" ++ show s ++ "_" ++ r) Nothing
      declLocalVariable (Just "static") (CArray [if d == 1 then 2*s else n | (d,n) <- zip b gridPerNode] commType) ("recv_buf" ++ show s ++ "_" ++ r') Nothing

defUtilFunctions :: BuildM ()
defUtilFunctions = do
  withMPI $ \mpiShape -> do
    dim <- view (globalEnvironment . dimension)
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

  axes <- view (globalEnvironment . axesNames)
  let toPosBody a =
        statement $ printf "return n.space_interval_%s*((i+n.offset_%s)%%n.total_grid_%s)" a a a
  for_ axes $ \a ->
    defGlobalFunction ("to_pos_"++a) [(CInt, "i"), (CRawType "Formura_Navi", "n")] CDouble (\_ -> toPosBody a)

defFormuraInit :: [(String,String)] -> BuildM ()
defFormuraInit navi = do
  defGlobalFunction "Formura_Init" [(CPtr CInt, "argc"),(CRawType "char ***", "argv"),(CRawType "Formura_Navi *","n")] CVoid (\_ -> initBody navi "MPI_COMM_WORLD")
  withMPI $ \_ ->
    () <$ defGlobalFunction "Formura_Custom_Init" [(CRawType "Formura_Navi *","n"),(CRawType "MPI_Comm", "comm")] CVoid (\_ -> initBody navi "comm")
  where
    initBody :: [(String,String)] -> String -> BuildM ()
    initBody fs comm = do
      dim <- view (globalEnvironment . dimension)
      withMPI $ \mpiShape -> do
        when (comm == "MPI_COMM_WORLD") $ call "MPI_Init" ["argc","argv"]
        declScopedVariable Nothing (CRawType "MPI_Comm") "cm" (Just comm)
        declScopedVariable Nothing CInt "size" Nothing
        declScopedVariable Nothing CInt "rank" Nothing
        call "MPI_Comm_size" ["cm", "&size"]
        call "MPI_Comm_rank" ["cm", "&rank"]
        raw $ printf "if(size != %d) {\nfprintf(stderr,\"Do not match the number of MPI process!\");\nexit(1);\n}" (product mpiShape)
        statement $ "int " ++ intercalate "," ["i" ++ show i | i <- [1..dim]]
        call "Formura_Decode_rank" ("rank":["&i"++show i | i <- [1..dim]])
      -- Navi構造体の初期化
      for_ fs $ \(f,v) -> statement $ "n->" ++ f ++ " = " ++ v

      call "Formura_Setup" ("*n":replicate dim "0")
      withFirstStep $ \_ -> call "Formura_First_Step" ["n"]

defFormuraFirstStep :: IRGraph -> BuildM ()
defFormuraFirstStep g = do
  blockOffset <- getBlockOffsets
  buff <- getVariable "first_input"
  rslt <- getVariable "first_output"
  s <- fromJust <$> view (globalEnvironment . envNumericalConfig . icSleeve0)
  defLocalFunction "Formura_First_Step_Kernel" ([(CPtr $ variableType buff, "buff"), (CPtr $ variableType rslt, "rslt"),(CRawType "Formura_Navi", "n")] ++ blockOffset) CVoid (mkKernel g s)
  defLocalFunction "Formura_First_Step" [(CRawType "Formura_Navi *", "n")] CVoid $ \_ ->
      noBlocking buff rslt s "Formura_First_Step_Kernel"

defFormuraFilter :: IRGraph -> BuildM ()
defFormuraFilter g = do
  blockOffset <- getBlockOffsets
  buff <- getVariable "filter_input"
  rslt <- getVariable "filter_output"
  s <- fromJust <$> view (globalEnvironment . envNumericalConfig . icFilterSleeve)
  defLocalFunction "Formura_Filter_Kernel" ([(CPtr $ variableType buff, "buff"), (CPtr $ variableType rslt, "rslt"),(CRawType "Formura_Navi", "n")] ++ blockOffset) CVoid (mkKernel g s)
  defLocalFunction "Formura_Filter" [(CRawType "Formura_Navi *", "n")] CVoid $ \_ ->
      noBlocking buff rslt s "Formura_Filter_Kernel"

defFormuraSetup :: BuildM ()
defFormuraSetup = do
  blockOffset <- getBlockOffsets
  defLocalFunction "Formura_Setup" ([(CRawType "Formura_Navi", "n")] ++ blockOffset) CVoid  $ \_ -> setupBody
  where
    setupBody = do
      globalData <- getGlobalData
      irg <- view irInitGraph
      mkKernel irg 0 [globalData,globalData]

defFormuraStep :: BuildM ()
defFormuraStep = do
  blockOffset <- getBlockOffsets
  buffType <- variableType <$> getVariable "step_input"
  rsltType <- variableType <$> getVariable "step_output"
  defLocalFunction "Formura_Step" ([(CPtr buffType, "buff"), (CPtr rsltType, "rslt"),(CRawType "Formura_Navi", "n")] ++ blockOffset) CVoid stepBody
  where
    stepBody :: [CVariable] -> BuildM ()
    stepBody args = do
      s <- view (globalEnvironment . envNumericalConfig . icSleeve)
      irg <- view irStepGraph
      mkKernel irg s args

defFormuraForward :: BuildM ()
defFormuraForward = do
  blockingType <- view (globalEnvironment . envNumericalConfig . icBlockingType)
  defGlobalFunction "Formura_Forward" [(CRawType "Formura_Navi *", "n")] CVoid (\_ -> forwardBody blockingType)
  where
    forwardBody NoBlocking = do
      buff <- getVariable "step_input"
      rslt <- getVariable "step_output"
      s <- view (globalEnvironment . envNumericalConfig . icSleeve)
      noBlocking buff rslt s "Formura_Step"
      withFilter $ \_ -> do
        filterInterval <- fromJust <$> view (globalEnvironment . envNumericalConfig . icFilterInterval)
        raw $ printf "if (n->time_step %% %d == 0) {" filterInterval
        call "Formura_Filter" ["n"]
        raw $ "}"
      statement "n->time_step += 1"
    forwardBody (TemporalBlocking gridPerBlock blockPerNode nt) = do
      s <- view (globalEnvironment . envNumericalConfig . icSleeve)
      dim <- view (globalEnvironment . dimension)
      globalData <- getGlobalData
      tmpFloor <- getVariable "tmp_floor"
      copy globalData tmpFloor empty (repeat (2*s*nt))
      -- 通信
      rs <- isendrecv globalData (s*nt)
      -- 即時計算可能なブロックと通信待ちブロックの分離
      let b0 = [(0,m-1-d) | (n,m) <- zip gridPerBlock blockPerNode, let d = 2*s*nt `div` n]
          bs = [[(if i == j then m-1-d else 0, if i > j then m-1-d else m) | (n,m,i) <- zip3 gridPerBlock blockPerNode [1..dim], let d = 2*s*nt `div` n] | j <- [1..dim]]
          update = updateWithTB gridPerBlock blockPerNode nt
      update b0
      waitAndCopy rs tmpFloor (s*nt)
      mapM_ update bs
      copy tmpFloor globalData empty empty
      axes <- view (globalEnvironment . axesNames)
      for_ axes $ \a -> statement $ printf "n->offset_%s = (n->offset_%s - %d + n->total_grid_%s)%%n->total_grid_%s" a a (s*nt) a a
      withFilter $ \_ -> do
        filterInterval <- fromJust <$> view (globalEnvironment . envNumericalConfig . icFilterInterval)
        raw $ printf "if (n->time_step %% %d == 0) {" filterInterval
        call "Formura_Filter" ["n"]
        raw $ "}"
      statement $ "n->time_step += " ++ show nt

noBlocking :: CVariable -> CVariable -> Int -> String -> BuildM ()
noBlocking buff rslt s kernelName = do
  globalData <- getGlobalData
  axes <- view (globalEnvironment . axesNames)
  dim <- view (globalEnvironment . dimension)
  -- 通信
  sendrecv globalData buff s
  copy globalData buff empty (repeat $ 2*s)
  -- 1ステップ更新
  call kernelName ([ref buff, ref rslt, "*n"] ++ replicate dim "0")
  for_ axes $ \a -> statement $ printf "n->offset_%s = (n->offset_%s - %d + n->total_grid_%s)%%n->total_grid_%s" a a s a a

updateWithTB :: [Int] -> [Int] -> Int -> [(Int,Int)] -> BuildM ()
updateWithTB gridPerBlock blockPerNode nt boundary = loopWith [("j" ++ show @Int i,l,u,1) | (i,(l,u)) <- zip [1..] boundary] $ \idx -> do
  dim <- view (globalEnvironment . dimension)
  s <- view (globalEnvironment . envNumericalConfig . icSleeve)
  tmpFloor <- getVariable "tmp_floor"
  tmpWalls <- forM [([i == j | j <- [1..dim]],i) | i <- [1..dim]] $ \(flag,i) -> do
    let gs = [if b then 2*s else n+2*s | (n,b) <- zip gridPerBlock flag]
    tmpWall <- getVariable ("tmp_wall_" ++ show i)
    return (flag, gs, tmpWall)
  buff <- getVariable "step_input"
  rslt <- getVariable "step_output"
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
        stack <>= [mkIdent f buff (idx' <> toIdx [if b then n else 0 | (b,n) <- zip flag gridPerBlock]) @= mkIdent f tmpWall (idx0 >< idx') | f <- (getFields tmpWall), f `elem` (getFields buff)]
      return ()
--   - 1段更新
    call "Formura_Step" ([ref buff, ref rslt,"*n"] ++ fromIdx floorOffset)
--   - 壁の書き出し
    for_ tmpWalls $ \(flag, gs, tmpWall) -> do
      let idx0 = (toIdx [i | (i,b) <- zip (fromIdx idx) flag, not b]) >< it
      loop gs $ \idx' ->
        stack <>= [mkIdent f tmpWall (idx0 >< idx')  @= mkIdent f buff idx' | f <- (getFields buff), f `elem` (getFields tmpWall)]
      return ()
-- - 床の書き出し
  copy rslt tmpFloor empty floorOffset

defFormuraFinalize :: BuildM ()
defFormuraFinalize =
  defGlobalFunction "Formura_Finalize" [] CVoid $ \_ ->
    withMPI $ \_ -> call "MPI_Finalize" []

mkKernel :: IRGraph -> Int -> [CVariable] -> BuildM ()
mkKernel irg sleeve args = do
  let tmps = tmpArrays irg
      ks = kernels irg
      isTmp n = n `elem` [n0 | (n0,_,_) <- tmps]
  let outputSize = getSize $ variableType $ args !! 1
      inputSize = map (+ (2*sleeve)) outputSize
  defTmpArrays inputSize tmps
  for_ (M.toAscList ks) $ \(_,mm) -> genKernel args isTmp inputSize mm

defTmpArrays :: [Int] -> [(String,OMNodeType,Int)] -> BuildM ()
defTmpArrays inputSize ts = for_ ts $ \(n,t,s) ->
  declScopedVariable (Just "static") (mapTmpType s t) n Nothing
  where
    mapTmpType :: Int -> OMNodeType -> CType
    mapTmpType _ (ElemType "Rational") = CDouble
    mapTmpType _ (ElemType "void") = CVoid
    mapTmpType _ (ElemType "double") = CDouble
    mapTmpType _ (ElemType "float") = CFloat
    mapTmpType _ (ElemType "int") = CInt
    mapTmpType _ (ElemType x) = CRawType x
    mapTmpType ds (GridType _ x) = CArray [s-2*ds | s <- inputSize] (mapTmpType ds x)
    mapTmpType _ _ = error "Invalid type"

genKernel :: [CVariable] -> (String -> Bool) -> [Int] -> (Int, MMNode) -> BuildM ()
genKernel args isTmp inputSize (s, Node mm _ _) =
  loop [s'-2*s | s' <- inputSize] $ \idx -> sequence_ [genMicroInst args idx s mmid mi mt isTmp | (mmid, Node mi mt _) <- M.toAscList mm]

genMicroInst :: [CVariable] -> Idx -> Int -> MMNodeID -> MicroInstruction -> MicroNodeType -> (String -> Bool) -> BuildM ()
genMicroInst args idx _ _ (Store n x) _ isTmp | isTmp n = stack <>= [(n ++ show idx) @= formatNode x]
                                              | otherwise = stack <>= [(mkIdent n (args !! 1) idx) @= formatNode x]
genMicroInst args idx s mmid mi mt isTmp = do
  axes <- view (globalEnvironment . axesNames)
  let decl x = declScopedVariable Nothing (mapType mt) (formatNode mmid) (Just x) >> return ()
  decl $ case mi of
    (LoadCursorStatic d n) | isTmp n   -> n ++ show (idx <> (toIdx . toList $ d))
                           | otherwise -> mkIdent n (args !! 0) (idx <> (toIdx . toList $ d + pure s))
    -- (LoadCursor d oid) -> tmpName oid ++ show (idx <> (toIdx . toList $ d + pure (s-(sizeTable M.! oid))))
    (Imm r) -> show (realToFrac r :: Double)
    (Uniop op a) | "external-call" `isPrefixOf` op -> (fromJust $ stripPrefix "external-call/" op) ++ "(" ++ formatNode a ++ ")"
                 | otherwise -> op ++ formatNode a
    (Binop op a b) | op == "**" -> "pow(" ++ formatNode a ++ "," ++ formatNode b ++ ")"
                   | otherwise -> formatNode a ++ op ++ formatNode b
    (Triop _ a b c) -> formatNode a ++ "?" ++ formatNode b ++ ":" ++ formatNode c
    LoadIndex i -> (fromIdx idx) !! i ++ "+n.offset_" ++ (axes !! i) ++ "+block_offset_" ++ show (i+1)
    -- Naryop は廃止かもなので、実装を待つ
    -- Naryop op xs -> undefined
    x -> error $ "Unimplemented for keyword: " ++ show x
  where
    mapType :: MicroNodeType -> CType
    mapType (ElemType "Rational") = CDouble
    mapType (ElemType "void")     = CVoid
    mapType (ElemType "double")   = CDouble
    mapType (ElemType "float")    = CFloat
    mapType (ElemType "int")      = CInt
    mapType (ElemType x)          = CRawType x
    mapType _                     = error "Invalid type"

formatNode :: Show a => a -> String
formatNode i = "a" ++ show i
