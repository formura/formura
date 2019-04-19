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

import System.Exit

import Formura0.Frontend.Lexer
import Formura0.Frontend.OptionParser
import Formura0.Frontend.Parser
import Formura0.Syntax

getProgram :: FilePath -> IO Program
getProgram fn = do
  program0 <- parse <$> readFile fn
  case program0 of
    Left err   -> die err
    Right prog -> return prog

parse :: String -> Either String Program
parse s = runAlex s happyParser
