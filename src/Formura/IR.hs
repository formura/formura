{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Formura.IR where

import Control.Lens hiding (op)
import           Data.Data
import Data.List
import qualified Data.Map as M
import           Text.Read (Read(..))

import Formura.Language.Combinator (Matches)
import Formura.GlobalEnvironment
import Formura.OrthotopeMachine.Graph
import Formura.Syntax

newtype IRNodeID = IRNodeID Int
  deriving (Eq, Ord, Num, Data)

instance Show IRNodeID where
  showsPrec n (IRNodeID x) = showsPrec n x

instance Read IRNodeID where
  readPrec = fmap IRNodeID  readPrec

data IRGraph = IRGraph
  { tmpArrays :: [(String,OMNodeType,Int)]
  , kernels :: M.Map IRNodeID (Int,MMNode)
  }

-- IRProgram is MMProgram whose manifest nodes are merged
data IRProgram = IRProgram
  { _irGlobalEnvironment :: GlobalEnvironment
  , _irInitGraph :: IRGraph
  , _irFirstStepGraph :: Maybe IRGraph
  , _irFilterGraph :: Maybe IRGraph
  , _irStepGraph :: IRGraph
  , _irStateSignature :: M.Map IdentName TypeExpr
  }

makeClassy ''IRProgram

instance HasGlobalEnvironment IRProgram where
  globalEnvironment = irGlobalEnvironment


genIRProgram :: MMProgram -> IRProgram
genIRProgram mmp =
  IRProgram { _irGlobalEnvironment = mmp ^. omGlobalEnvironment
            , _irInitGraph = mkIRGraph $ mmp ^. omInitGraph
            , _irFirstStepGraph = mkIRGraph <$> mmp ^. omFirstStepGraph
            , _irFilterGraph = mkIRGraph <$> mmp ^. omFilterGraph
            , _irStepGraph = mkIRGraph $ mmp ^. omStepGraph
            , _irStateSignature = mmp ^. omStateSignature
            }

mkIRGraph :: MMGraph -> IRGraph
mkIRGraph mmg = IRGraph { tmpArrays = tmps
                        , kernels = g
                        }
  where
    tmpPrefix = "formura_mn_"
    tmpName oid = tmpPrefix ++ show oid

    sizeTable = calcSizes mmg
    getSize i = sizeTable M.! i

    tmps = gatherTmps tmpName getSize mmg
    mmg' = insertMNSotres tmpName mmg

    -- 一時変数の特定とStoreの発行 & LoadCursorをLoadCursorStaticに
    -- マージ可能なMMNodeを集める
    -- マージする
    g = M.fromList $ zipWith (\i n -> (IRNodeID i,n)) [0..] $ map (fmap (updateLoadCursor tmpName getSize . mergeMMNodes)) $ groupMMNodes getSize mmg'

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

isVoid :: ElemTypeF `Matches` s => s -> Bool
isVoid (ElemType "void") = True
isVoid _ = False

gatherTmps :: (OMNodeID -> String) -> (OMNodeID -> Int) -> MMGraph -> [(String,OMNodeType,Int)]
gatherTmps tmpName getSize mmg = M.foldrWithKey (\omid (Node _ omt _) ts -> if isVoid omt then ts else (tmpName omid, omt, getSize omid):ts) [] mmg

insertMNSotres :: (OMNodeID -> String) -> MMGraph -> MMGraph
insertMNSotres tmpName = M.mapWithKey (\omid n@(Node mm omt x) -> if isVoid omt then n else (Node (insertMNStore omid mm) (ElemType "void") x))
  where
    insertMNStore :: OMNodeID -> MMInstruction -> MMInstruction
    insertMNStore oid mm = M.insert (MMNodeID mmid) node mm
      where mmid = M.size mm
            node = Node (Store (tmpName oid) (MMNodeID $ mmid-1)) (ElemType "void") []

-- 各Manifest node間の依存関係を調べ、依存関係がないもの同士をまとめる
groupMMNodes :: (OMNodeID -> Int) -> MMGraph -> [(Int, [(OMNodeID, MMNode)])]
groupMMNodes getSize mmg = foldr (\x acc -> add x acc) [] $ M.toAscList mmg
  where
    loadFrom mm ids = let f (LoadCursor _ oid) = oid `elem` ids
                          f _ = False
                       in not $ M.null $ M.filter (\(Node inst _ _) -> f inst) mm
    dep (_,Node mm _ _) g = mm `loadFrom` (map fst g)
    add x [] = [(getSize $ fst x,[x])]
    add x ((s,g):gs) | dep x g = (s,g):(add x gs)
                     | s /= (getSize $ fst x) = (s,g):(add x gs)
                     | otherwise = (s,g ++ [x]):gs

mergeMMNodes :: [(OMNodeID, MMNode)] -> MMNode
mergeMMNodes = foldl' (\(Node mm0 t a) (_,Node mm1 _ _) -> Node (merge mm0 mm1) t a) (Node M.empty (ElemType "void") [])
  where
    getMaxKey = maximum . M.keys
    updateKey x = M.mapKeys (+x)
    merge m0 m1 = let offset = getMaxKey m0
                      m1' = updateKey offset m1
                  in mergeMMInstruction m0 m1'

-- m0 に m1 の各ノードを追加していく
-- このとき、同じノードがm0に存在していれば、m1にあるキーを書き換える
mergeMMInstruction :: MMInstruction -> MMInstruction -> MMInstruction
mergeMMInstruction m0 m1 = worker m0 $ M.toAscList m1
  where
    exist t m = let m' = M.filter (==t) m in if null m' then Nothing else Just (head $ M.keys m')
    fixKey old new n = case n ^. nodeInst of 
                         Uniop op k | k == old -> n & nodeInst .~ Uniop op new
                         Binop op k1 k2 | k1 == old && k2 == old -> n & nodeInst .~ Binop op new new
                                        | k1 == old -> n & nodeInst .~ Binop op new k2
                                        | k2 == old -> n & nodeInst .~ Binop op k1 new
                         Triop op k1 k2 k3 | (k1,k2,k3) == (old,old,old) -> n & nodeInst .~ Triop op new new new
                                           | (k1,k2) == (old,old) -> n & nodeInst .~ Triop op new new k3
                                           | (k2,k3) == (old,old) -> n & nodeInst .~ Triop op k1 new new
                                           | (k3,k1) == (old,old) -> n & nodeInst .~ Triop op new k2 new
                                           | k1 == old -> n & nodeInst .~ Triop op new k2 k3
                                           | k2 == old -> n & nodeInst .~ Triop op k1 new k3
                                           | k3 == old -> n & nodeInst .~ Triop op k1 k2 new
                         _ -> n
    worker m [] = m
    worker m ((mid,inst):ms) = case exist inst m of
                                 Nothing -> worker (M.insert mid inst m) ms
                                 Just k -> worker m (map (\(i,n) -> (mid, fixKey i k n)) ms)

-- LoadCursor を LoadCursorStatic に変更する
updateLoadCursor :: (OMNodeID -> String) -> (OMNodeID -> Int) -> MMNode -> MMNode
updateLoadCursor tmpName getSize (Node mm omt x) = Node (M.map update mm) omt x
  where
    update (Node (LoadCursor s oid) t a) = Node (LoadCursorStatic (s - pure (getSize oid)) (tmpName oid)) t a
    update n = n
