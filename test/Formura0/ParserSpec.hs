{-# LANGUAGE OverloadedStrings #-}
module Formura0.ParserSpec (spec) where

import Test.Hspec

import Formura.Vec
import Formura0.Frontend (parse)
import Formura0.Frontend.Lexer
import Formura0.Syntax

spec :: Spec
spec = do
  describe "Unit test for parse" $ do
    it "check a simple statement" $
      parse "x = 1.0" `shouldBe` Right (Program [Subst (AlexPn 2 1 3) (IdentL "x") (Imm 1.0)])
    it "check a typed statement" $
      parse "double :: x = 1.0" `shouldBe` Right (Program [TypeDecl (AlexPn 12 1 13) (ModifiedTypeExpr [] (ElemType "double")) (IdentL "x"),Subst (AlexPn 12 1 13) (IdentL "x") (Imm 1.0)])
    it "check multi statements" $
      parse "double :: x = 1.0\ny = x" `shouldBe` Right (Program [TypeDecl (AlexPn 12 1 13) (ModifiedTypeExpr [] (ElemType "double")) (IdentL "x"),Subst (AlexPn 12 1 13) (IdentL "x") (Imm 1.0), Subst (AlexPn 20 2 3) (IdentL "y") (IdentR "x")])
    it "check special declarations" $
      parse "dimension :: 3\naxes :: x, y, z\ngrid_struct_type_name :: formura_data\nhoge=3.0" `shouldBe` Right (Program [SpecialDecl (AlexPn 0 1 1) $ DimensionDeclaration 3, SpecialDecl (AlexPn 15 2 1) $ AxesDeclaration ["x","y","z"], SpecialDecl (AlexPn 31 3 1) $ GridStructTypeNameDeclaration "formura_data",Subst (AlexPn 73 4 5) (IdentL "hoge") (Imm 3.0)])
    it "check a grid type declaration" $
      parse "double[] :: r = 0.0" `shouldBe` Right (Program [TypeDecl (AlexPn 14 1 15) (ModifiedTypeExpr [] (GridType (Vec []) (ElemType "double"))) (IdentL "r"), Subst (AlexPn 14 1 15) (IdentL "r") (Imm 0.0)])

