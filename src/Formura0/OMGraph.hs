module Formura0.OMGraph where

import Formura.Vec
import Formura0.Syntax

type OMID = Int

data OMInst = Load !(Vec Int) !OMID
            | Store !IdentName !OMID
            | LoadIndex !Int
            | Uniop !IdentName !OMID
            | Binop !IdentName !OMID !OMID
            | Triop !IdentName !OMID !OMID !OMID
            | Imm !Rational
            | Ident !IdentName
            | Call !IdentName ![OMID] ![OMID]


-- OMGraph に関する操作
--  - AST から Graph を構築する
--  - Graph 上でカーネルを分割する (サブグラフを作る)
--  - Graph 上のシフト操作を持ち上げる
--  - 共通部分式の削除
