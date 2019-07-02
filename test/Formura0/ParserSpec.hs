{-# LANGUAGE OverloadedStrings #-}
module Formura0.ParserSpec (spec) where

import Data.Either
import Test.Hspec

import Formura0.Frontend (parse)
import Formura0.Frontend.Lexer
import Formura0.Syntax

spec :: Spec
spec = do
  describe "Unit test" $ do
    it "check a typeless statement" $
      parse "a = 1" `shouldBe` Right ([VarDecl (AlexPn 2 1 3) (Ident "a") (Imm' 1)])
    it "check a typed statement" $
      parse "double :: a = 1" `shouldBe` Right ([TypeDecl (AlexPn 7 1 8) (ModifiedType [] $ Ident "double") (Ident "a"), VarDecl (AlexPn 12 1 13) (Ident "a") (Imm' 1)])
    it "check a variable declaration" $
      parse "double :: a" `shouldBe` Right ([TypeDecl (AlexPn 7 1 8) (ModifiedType [] $ Ident "double") (Ident "a")])
    it "check the tuple syntax" $
      parse "(real,real) :: (x,y) = (1,a)" `shouldBe` Right [TypeDecl (AlexPn 12 1 13) (ModifiedType [] $ Tuple [Ident "real", Ident "real"]) (Tuple [Ident "x", Ident "y"]), VarDecl (AlexPn 21 1 22) (Tuple [Ident "x", Ident "y"]) (Tuple' [Imm' 1, Ident' "a"])]
    it "check multiple statements" $
      parse "double :: x = 1.0\ny = x" `shouldBe` Right [TypeDecl (AlexPn 7 1 8) (ModifiedType [] $ Ident "double") (Ident "x"),VarDecl (AlexPn 12 1 13) (Ident "x") (Imm' 1.0), VarDecl (AlexPn 20 2 3) (Ident "y") (Ident' "x")]
    it "check the special declarations" $
      parse "dimension :: 3\naxes :: x, y, z\ngrid_struct_type_name :: formura_data" `shouldBe` Right [SpcDecl (AlexPn 0 1 1) (Dimension 3), SpcDecl (AlexPn 15 2 1) (Axes ["x","y","z"]),SpcDecl (AlexPn 31 3 1) (GSTypeName "formura_data")]
    it "check type modifiers" $
      parse "extern function :: sqrt\nmanifest :: a = 1" `shouldBe` Right [TypeDecl (AlexPn 16 1 17) (ModifiedType [TMExtern] $ Ident "function") (Ident "sqrt"), TypeDecl (AlexPn 33 2 10) (ModifiedType [TMManifest] None) (Ident "a"), VarDecl (AlexPn 38 2 15) (Ident "a") (Imm' 1)]
    it "check a grid type" $
      parse "double[] :: r = 0" `shouldBe` Right [TypeDecl (AlexPn 9 1 10) (ModifiedType [] (Grid [] (Ident "double"))) (Ident "r"), VarDecl (AlexPn 14 1 15) (Ident "r") (Imm' 0)]
    it "check a grid value" $
      parse "r[x] = r[x+1/2]" `shouldBe` Right [VarDecl (AlexPn 5 1 6) (Grid [NPlusK "x" 0] (Ident "r")) (Grid' [NPlusK "x" (1/2)] (Ident' "r"))]

    it "check arithmetic expressions" $
      parse "y = -4*x**2 + a*x - 3/2\nz = +(a,b) + (3*(1,2))" `shouldBe`
        Right [ VarDecl (AlexPn 2 1 3) (Ident "y")
                                       (Binop' Sub (Binop' Add (Binop' Mul (Uniop' Minus (Imm' 4)) (Binop' Pow (Ident' "x") (Imm' 2)))
                                                               (Binop' Mul (Ident' "a") (Ident' "x")))
                                                   (Binop' Div (Imm' 3) (Imm' 2)))
              , VarDecl (AlexPn 26 2 3) (Ident "z")
                                        (Binop' Add (Uniop' Plus (Tuple' [Ident' "a",Ident' "b"]))
                                                    (Binop' Mul (Imm' 3) (Tuple' [Imm' 1,Imm' 2])))
              ]
    it "check let-in expression" $
      parse "y = let a = 1\nb=2\nin a**2 + b**2" `shouldBe`
        Right [ VarDecl (AlexPn 2 1 3) (Ident "y")
                                       (Let' [VarDecl (AlexPn 10 1 11) (Ident "a") (Imm' 1)
                                             ,VarDecl (AlexPn 15 2 2) (Ident "b") (Imm' 2)]
                                             (Binop' Add (Binop' Pow (Ident' "a") (Imm' 2)) (Binop' Pow (Ident' "b") (Imm' 2))))
              ]
    it "check lambda expression" $
      parse "f = fun(a,(b,c)) a*b + c" `shouldBe`
        Right [ VarDecl (AlexPn 2 1 3) (Ident "f") (Lambda' [Ident "a",Tuple [Ident "b",Ident "c"]] (Binop' Add (Binop' Mul (Ident' "a") (Ident' "b")) (Ident' "c"))) ]
    it "check if expression" $
      parse "x = if a then b else c + d" `shouldBe`
        Right [ VarDecl (AlexPn 2 1 3) (Ident "x") (If' (Ident' "a") (Ident' "b") (Binop' Add (Ident' "c") (Ident' "d"))) ]
    it "check function application" $
      parse "y = f x\nz = g(1.0,a)\nw = (f.g)(1.0,a)" `shouldBe`
        Right [ VarDecl (AlexPn 2 1 3) (Ident "y") (App' (Ident' "f") (Ident' "x"))
              , VarDecl (AlexPn 10 2 3) (Ident "z") (App' (Ident' "g") (Tuple' [Imm' 1.0, Ident' "a"]))
              , VarDecl (AlexPn 23 3 3) (Ident "w") (App' (Lambda' [Ident "x"] (App' (Ident' "f") (App' (Ident' "g") (Ident' "x"))))
                                                          (Tuple' [Imm' 1,Ident' "a"]))
              ]
    it "check function application chain" $
      parse "x = fun(a) f(a)(b)" `shouldBe`
        Right [ VarDecl (AlexPn 2 1 3) (Ident "x") (Lambda' [Ident "a"] (App' (App' (Ident' "f") (Ident' "a")) (Ident' "b"))) ]
    it "check function definition" $
      parse "begin function x = f(a,b,c)\nx = a*b + c\nend function\n\nbegin function (u,v) = init()\ndouble :: u = 0, v = 0\nend function" `shouldBe`
        Right [ TypeDecl (AlexPn 0 1 1) (ModifiedType [] None) (Ident "f")
              , VarDecl (AlexPn 0 1 1) (Ident "f") (Lambda' [Ident "a",Ident "b",Ident "c"]
                                                            (Let' [ VarDecl (AlexPn 30 2 3) (Ident "x") (Binop' Add (Binop' Mul (Ident' "a") (Ident' "b")) (Ident' "c"))]
                                                                  (Ident' "x")))
              , TypeDecl (AlexPn 54 5 1) (ModifiedType [] None) (Ident "init")
              , VarDecl (AlexPn 54 5 1) (Ident "init") (Lambda' [] (Let' [ TypeDecl (AlexPn 91 6 8) (ModifiedType [] (Ident "double")) (Ident "u")
                                                                                , VarDecl (AlexPn 96 6 13) (Ident "u") (Imm' 0)
                                                                                , TypeDecl (AlexPn 91 6 8) (ModifiedType [] (Ident "double")) (Ident "v")
                                                                                , VarDecl (AlexPn 103 6 20) (Ident "v") (Imm' 0)
                                                                                ]
                                                                                (Tuple' [Ident' "u", Ident' "v"])))
              ]
    it "check logical expression" $
      parse "max = fun(x,y) if x > y then x else y\nz = if (x >= 0) && (x <= 10) then 1 else 0" `shouldBe`
        Right [ VarDecl (AlexPn 4 1 5) (Ident "max") (Lambda' [Ident "x",Ident "y"] (If' (Binop' Gt (Ident' "x") (Ident' "y")) (Ident' "x") (Ident' "y")))
              , VarDecl (AlexPn 40 2 3) (Ident "z") (If' (Binop' And (Binop' GEq (Ident' "x") (Imm' 0)) (Binop' LEq (Ident' "x") (Imm' 10))) (Imm' 1) (Imm' 0))
              ]
    it "check compound statements" $
      parse "double :: a, b = 1, c" `shouldBe`
        Right [ TypeDecl (AlexPn 7 1 8) (ModifiedType [] (Ident "double")) (Ident "a")
              , TypeDecl (AlexPn 7 1 8) (ModifiedType [] (Ident "double")) (Ident "b")
              , VarDecl (AlexPn 15 1 16) (Ident "b") (Imm' 1)
              , TypeDecl (AlexPn 7 1 8) (ModifiedType [] (Ident "double")) (Ident "c")
              ]
    it "check comment" $ do
      parse "z = 3\n\n## hoge\n#foo\ndouble :: x = 1 # bar\ny = 2" `shouldBe`
        Right [ VarDecl (AlexPn 2 1 3) (Ident "z") (Imm' 3)
              , TypeDecl (AlexPn 27 5 8) (ModifiedType [] (Ident "double")) (Ident "x")
              , VarDecl (AlexPn 32 5 13) (Ident "x") (Imm' 1)
              , VarDecl (AlexPn 44 6 3) (Ident "y") (Imm' 2)
              ]
    it "check empty statement" $
      parse "x = 1\n\n\ny = 2" `shouldBe`
        Right [ VarDecl (AlexPn 2 1 3) (Ident "x") (Imm' 1)
              , VarDecl (AlexPn 10 4 3) (Ident "y") (Imm' 2)
              ]
    it "check tuples" $
      parse "y = (x)\nz = (a,b)" `shouldBe`
        Right [ VarDecl (AlexPn 2 1 3) (Ident "y") (Ident' "x")
              , VarDecl (AlexPn 10 2 3) (Ident "z") (Tuple' [Ident' "a",Ident' "b"])
              ]

  describe "Full code" $ do
    it "check 3d diffusion" $ do
      code <- readFile "./examples/diffusion3/diffusion3.fmr"
      parse code `shouldSatisfy` isRight
    it "check 1d hllc" $ do
      code <- readFile "./sample/1d_hllc/hllc.fmr"
      parse code `shouldSatisfy` isRight
    it "check 3d hllc" $ do
      code <- readFile "./sample/3d_hllc/3d_hllc.fmr"
      parse code `shouldSatisfy` isRight
    it "check 1d hlld" $ do
      code <- readFile "./sample/mhd_1d_hlld/mhd_hlld.fmr"
      parse code `shouldSatisfy` isRight
    it "check 3d hlld" $ do
      code <- readFile "./sample/mhd_3d_hlld/mhd_3d_hlld.fmr"
      parse code `shouldSatisfy` isRight
