{-# LANGUAGE OverloadedStrings #-}
module Formura0.ParserSpec (spec) where

import Test.Hspec

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


