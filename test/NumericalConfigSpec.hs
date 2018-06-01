{-# LANGUAGE OverloadedStrings #-}
module NumericalConfigSpec (spec) where

import qualified Data.ByteString.Char8 as B
import Data.Either (isLeft)
import Test.Hspec

import Formura.Vec
import Formura.NumericalConfig

spec :: Spec
spec = do
  describe "Valid cases" $ do
    it "1d config" $ do
      let cfg = B.unlines [ "length_per_node: [1.0]"
                          , "grid_per_node: [10]"
                          , "grid_per_block: [10]"
                          , "mpi_shape: [1]"
                          , "temporal_blocking_interval: 5"
                          ]
          cfg' = NumericalConfig
                  { _ncLengthPerNode = Vec [1.0]
                  , _ncGridPerNode = Vec [10]
                  , _ncMPIShape = Vec [1]
                  , _ncGridPerBlock = Vec [10]
                  , _ncTemporalBlockingInterval = 5
                  , _ncFilterInterval = Nothing
                  }
      decodeConfig cfg `shouldBe` (Right cfg')
    it "1d config with filter" $ do
      let cfg = B.unlines [ "length_per_node: [1.0]"
                          , "grid_per_node: [10]"
                          , "grid_per_block: [10]"
                          , "mpi_shape: [1]"
                          , "temporal_blocking_interval: 5"
                          , "filter_interval: 10"
                          ]
          cfg' = NumericalConfig
                  { _ncLengthPerNode = Vec [1.0]
                  , _ncGridPerNode = Vec [10]
                  , _ncMPIShape = Vec [1]
                  , _ncGridPerBlock = Vec [10]
                  , _ncTemporalBlockingInterval = 5
                  , _ncFilterInterval = Just 10
                  }
      decodeConfig cfg `shouldBe` (Right cfg')
    it "3d config" $ do
      let cfg = B.unlines [ "length_per_node: [1.0,2.0,3.0]"
                          , "mpi_shape: [2,2,2]"
                          , "temporal_blocking_interval: 5"
                          , "grid_per_node: [10,10,10]"
                          , "grid_per_block: [10,10,10]"
                          ]
          cfg' = NumericalConfig
                  { _ncLengthPerNode = Vec [1.0,2.0,3.0]
                  , _ncGridPerNode = Vec [10,10,10]
                  , _ncMPIShape = Vec [2,2,2]
                  , _ncGridPerBlock = Vec [10,10,10]
                  , _ncTemporalBlockingInterval = 5
                  , _ncFilterInterval = Nothing
                  }
      decodeConfig cfg `shouldBe` (Right cfg')
  describe "Invalid cases" $ do
    it "don't exist MUST fields" $ do
      let cfg = B.unlines [ "length_per_node: [1.0]"
                          , "grid_per_node: [10]"
                          , "grid_per_block: [10]"
                          ]
      decodeConfig cfg `shouldSatisfy` isLeft
    it "has empty list" $ do
      let cfg = B.unlines [ "length_per_node: [1.0]"
                          , "grid_per_node: [10]"
                          , "grid_per_block: []"
                          , "mpi_shape: []"
                          , "temporal_blocking_interval: 5"
                          ]
      decodeConfig cfg `shouldSatisfy` isLeft
    it "has a negative temporal_blocking_interval" $ do
      let cfg = B.unlines [ "length_per_node: [1.0]"
                          , "grid_per_node: [10]"
                          , "grid_per_block: [10]"
                          , "mpi_shape: [1]"
                          , "temporal_blocking_interval: -5"
                          ]
      decodeConfig cfg `shouldSatisfy` isLeft
    it "has a negative filter_interval" $ do
      let cfg = B.unlines [ "length_per_node: [1.0]"
                          , "grid_per_node: [10]"
                          , "grid_per_block: [10]"
                          , "mpi_shape: [1]"
                          , "temporal_blocking_interval: 5"
                          , "filter_interval: -10"
                          ]
      decodeConfig cfg `shouldSatisfy` isLeft
