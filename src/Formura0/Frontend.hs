{-# LANGUAGE OverloadedStrings #-}
module Formura0.Frontend
  ( getOption
  , getProgram
  , parse
  ) where

-- * 概要
-- 必要なデータを読み込み、検証を行う
--
-- 読み込むもの
-- - コマンドラインオプション
-- - 設定データ
-- - プログラム
--
-- それぞれ、サブモジュールに分割し実装する

import Control.Monad
import Control.Applicative
import System.Exit

import Formura0.Frontend.Lexer
import Formura0.Frontend.OptionParser
import Formura0.Frontend.Parser
import Formura0.Syntax
import Formura0.Utils
import Formura0.Vec

getProgram :: FilePath -> IO Program
getProgram fn = do
  program0 <- (fixStatements <=< parse) <$> readFile fn
  case program0 of
    Left err   -> die err
    Right prog -> return prog

parse :: String -> Either String [Statement0]
parse s = runAlex s happyParser

fixStatements :: [Statement0] -> Either String [Statement]
fixStatements = mapM fixStatement
  where
    fixStatement :: Statement0 -> Either String Statement
    fixStatement (TypeDecl p t l) = formatError p $ TypeDecl p <$> fixTExp t <*> fixLExp l
    fixStatement (VarDecl p l r)  = formatError p $ VarDecl p <$> fixLExp l <*> fixRExp r
    fixStatement (SpcDecl p d)    = formatError p $ return $ SpcDecl p d

fixTExp :: ModifiedType Exp -> Either String (ModifiedType TExp)
fixTExp (ModifiedType ms t) = ModifiedType ms <$> fixTExp' t
  where
    fixTExp' (Ident n)  = return $ IdentT n
    fixTExp' (Tuple xs) = TupleT <$> (mapM fixTExp' xs)
    fixTExp' (Grid i e) = GridT <$> fixIndexT i <*> (fixTExp' e)
    fixTExp' None       = return $ SomeType

fixIndexT :: [NPlusK] -> Either String (Vec Rational)
fixIndexT npk = mapM fixNpK =<< fixIndexR npk
  where fixNpK (NPlusK "" x) = return x
        fixNpK _             = Left "invalid exp in grid type index"

fixLExp :: Exp -> Either String LExp
fixLExp (Ident n)  = return $ IdentL n
fixLExp (Tuple xs) = TupleL <$> (mapM fixLExp xs)
fixLExp (Grid i e) = GridL <$> fixIndexL i <*> fixLExp e
fixLExp None       = Left "invalid exp in LExp"

fixIndexL :: [NPlusK] -> Either String (Vec IdentName)
fixIndexL npk = mapM fixNpK =<< fixIndexR npk
  where fixNpK (NPlusK n x) | x == 0 = return n
        fixNpK _            = Left "invalid exp in LHS grid index"

fixRExp :: Exp' -> Either String RExp
fixRExp (Ident' n)        = return $ IdentR n
fixRExp (Imm' x)          = return $ ImmR x
fixRExp (Tuple' xs)       = TupleR <$> mapM fixRExp xs
fixRExp (Grid' i e)       = GridR <$> fixIndexR i <*> fixRExp e
fixRExp (Uniop' op e)     = UniopR op <$> fixRExp e
fixRExp (Binop' op e1 e2) = BinopR op <$> fixRExp e1 <*> fixRExp e2
fixRExp (Let' xs e)       = LetR <$> fixStatements xs <*> fixRExp e
fixRExp (Lambda' l r)     = LambdaR <$> fixLExp l <*> fixRExp r
fixRExp (If' e1 e2 e3)    = IfR <$> fixRExp e1 <*> fixRExp e2 <*> fixRExp e3
fixRExp (App' e1 e2)      = AppR <$> fixRExp e1 <*> fixRExp e2

fixIndexR :: [NPlusK] -> Either String (Vec NPlusK)
fixIndexR npk = return $ Vec (ZipList npk')
  where
    npk' = take 3 $ npk ++ replicate 3 (NPlusK "" 0)

formatError :: AlexPosn -> Either String Statement -> Either String Statement
formatError _ (Right s)  = Right s
formatError p (Left err) = Left $ "error:" ++ formatPos p ++ err
