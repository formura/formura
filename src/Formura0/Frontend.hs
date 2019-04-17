module Formura0.Frontend
  ( getOption
  , getProgram
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

import Formura0.Frontend.OptionParser
import Formura0.Syntax

getProgram :: IO Program
getProgram = undefined
