module Main where

import Formura0.Frontend

-- * Structure of formura0
-- formura0 は formura の新しい実装である。
-- formura の持つ複雑なAST定義を修正し、実行時のパフォーマンスを向上させることを目標としている。
--
-- formura0 は、次のような構造を持つ。
--
-- code.fmr (Text)
--  ↓ Formura0.Frontend.parse
-- AST      (Program): 型チェック済の構文木 + 検証済設定データ
--  ↓ Formura0.Middleend.translate
-- OM       (Graph): OMの命令グラフ
--  ↓ Formura0.Middleend.optimize
-- OM       (Graph)
--  ↓ Formura0.Backend.generate
-- code.c   (Text)
--
-- formura0 のモジュール構造は次のようになっている。
--
-- Formura0
--   - Syntax
--   - Config
--   - Option
--   - Graph
--   - Frontend
--     - Base
--     - Lexer
--     - Parser
--     - OptionParser
--     - ConfigParser
--     - TypeChecker
--   - Middleend
--     - Translater
--     - Optimizer
--   - Backend
--     - Generator
--

main :: IO ()
main = do
  opt <- getOption
  program <- getProgram (inputFilename opt)
  print program
