module Formura0.OptionParserSpec (spec) where

import Test.Hspec

import Formura0.Frontend.OptionParser (getOptionPure)
import Formura0.Option

spec :: Spec
spec = do
  describe "Unit test for getOptionPure" $ do
    it "only a file name" $
      getOptionPure ["hoge.fmr"] `shouldBe`
        Right (Option { inputFilename = "hoge.fmr", verbose = False, configFilename = Nothing })
    it "full" $
      getOptionPure ["hoge.fmr","--verbose","--nc","config.yaml"] `shouldBe`
        Right (Option { inputFilename = "hoge.fmr", verbose = True, configFilename = Just "config.yaml" })
