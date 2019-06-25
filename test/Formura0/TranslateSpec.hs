{-# LANGUAGE OverloadedStrings #-}
module Formura0.TranslateSpec (spec) where

import qualified Data.HashMap.Lazy as HM
import           Test.Hspec

import Formura0.Frontend.Lexer
import Formura0.Middleend.Translate
import Formura0.Syntax
import Formura0.Vec

spec :: Spec
spec = do
  describe "Tree" $ do
    it "check flatten" $
      flatten (Node [Leaf "a", Node [Leaf "b", Leaf "c"], Leaf "d"]) `shouldBe` (["a","b","c","d"] :: [String])
  describe "makeIdentTable" $ do
    it "check a simple case" $
      makeIdentTable [ VarDecl (AlexPn 1 1 1) (IdentL "x") (ImmR 1.0)
                     , VarDecl (AlexPn 2 2 2) (TupleL [IdentL "a",IdentL "b"]) (BinopR Add (IdentR "x") (IdentR "y"))]
        `shouldBe` Right (HM.fromList [("x", (AlexPn 1 1 1, [], ValueR $ ImmR 1.0)),("a", (AlexPn 2 2 2, [], ValueR $ AppR (BinopR Add (IdentR "x") (IdentR "y")) (ImmR 0))),("b", (AlexPn 2 2 2, [], ValueR $ AppR (BinopR Add (IdentR "x") (IdentR "y")) (ImmR 1)))])
    it "check a complex case" $
      makeIdentTable [ VarDecl (AlexPn 1 1 1) (TupleL [IdentL "a", TupleL [IdentL "b", IdentL "c"], IdentL "d"]) (IdentR "x")]
        `shouldBe` Right (HM.fromList [ ("a", (AlexPn 1 1 1, [], ValueR $ AppR (IdentR "x") (ImmR 0)))
                                      , ("b", (AlexPn 1 1 1, [], ValueR $ AppR (AppR (IdentR "x") (ImmR 1)) (ImmR 0)))
                                      , ("c", (AlexPn 1 1 1, [], ValueR $ AppR (AppR (IdentR "x") (ImmR 1)) (ImmR 1)))
                                      , ("d", (AlexPn 1 1 1, [], ValueR $ AppR (IdentR "x") (ImmR 2)))
                                      ])
  describe "findGlobalVariables" $ do
    it "check a simple case" $
      findGlobalVariables (HM.singleton "init" (AlexPn 1 1 1, [], ValueR $ LambdaR [] (LetR [TypeDecl (AlexPn 1 1 1) (ModifiedType [] (GridT (vec []) (IdentT "double"))) (IdentL "u"), VarDecl (AlexPn 1 1 1) (IdentL "u") (ImmR 0), TypeDecl (AlexPn 2 2 2) (ModifiedType [] (GridT (vec []) (IdentT "double"))) (IdentL "v"), VarDecl (AlexPn 2 2 2) (IdentL "v") (ImmR 0)] (TupleR [IdentR "u", IdentR "v"]))))
        `shouldBe` Right [("u", GridT (vec []) (IdentT "double")),("v", GridT (vec []) (IdentT "double"))]
