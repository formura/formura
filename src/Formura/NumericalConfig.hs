{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Formura.NumericalConfig where

import           Cases (snakify)
import           Control.Lens
import           Control.Exception
import           Data.Aeson.TH
import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Data
import           Data.Scientific
import           Data.Text.Lens (packed)
import qualified Data.Yaml as Y

import Formura.Vec

-- Config for user input
data NumericalConfig = NumericalConfig
  { _ncLengthPerNode :: Vec Scientific
  , _ncGridPerNode :: Vec Int
  , _ncGridPerBlock :: Vec Int
  , _ncTemporalBlockingInterval :: Int
  , _ncMPIShape :: Vec Int
  } deriving (Eq, Ord, Read, Show, Typeable, Data)

makeClassy ''NumericalConfig


$(deriveJSON (let toSnake = packed %~ snakify
              in defaultOptions { fieldLabelModifier = toSnake . drop 3
                                , constructorTagModifier = toSnake
                                , omitNothingFields = True
                                })
  ''NumericalConfig)

data ConfigException = ConfigException String
  deriving Eq

instance Show ConfigException where
  show (ConfigException err) = err

instance Exception ConfigException

decodeConfig :: ByteString -> Either ConfigException NumericalConfig
decodeConfig str = check =<< first ConfigException (Y.decodeEither str)
  where
    check :: NumericalConfig -> Either ConfigException NumericalConfig
    check cfg | null (cfg ^. ncLengthPerNode) = Left $ ConfigException "length_per_node should be a nonempty list"
              | null (cfg ^. ncGridPerNode) = Left $ ConfigException "grid_per_node should be a nonempty list"
              | null (cfg ^. ncGridPerBlock) = Left $ ConfigException "grid_per_block should be a nonempty list"
              | null (cfg ^. ncMPIShape) = Left $ ConfigException "mpi_shape should be a nonempty list"
              | (cfg ^. ncTemporalBlockingInterval) < 1 = Left $ ConfigException "temporal_blocking_interval should be a positive integer"
              | otherwise = Right cfg

readConfig :: FilePath -> IO NumericalConfig
readConfig fn = do
  con <- B.readFile fn
  case decodeConfig con of
    Left err -> throwIO err
    Right cfg -> return cfg

-- Config for code generation
data InternalConfig = InternalConfig
  { _icLengthPerNode :: Vec Scientific
  , _icGridPerNode :: Vec Int
  , _icBlockPerNode :: Vec Int
  , _icGridPerBlock :: Vec Int
  , _icTemporalBlockingInterval :: Int
  , _icSleeve :: Int
  , _icMPIShape :: Vec Int
  } deriving (Eq, Ord, Read, Show, Typeable, Data)

makeClassy ''InternalConfig

defaultInternalConfig :: InternalConfig
defaultInternalConfig = InternalConfig
  { _icLengthPerNode = Vec []
  , _icGridPerNode = Vec []
  , _icBlockPerNode = Vec []
  , _icGridPerBlock = Vec []
  , _icTemporalBlockingInterval = 1
  , _icSleeve = 1
  , _icMPIShape = Vec []
  }

convertConfig :: Int -> NumericalConfig -> Either ConfigException InternalConfig
convertConfig s nc = check ic
  where
    totalGrids = (nc ^. ncGridPerNode) + pure (2*s*(nc ^. ncTemporalBlockingInterval))
    ms = liftVec2 (div) totalGrids (nc ^. ncGridPerBlock)
    ms' = liftVec2 (mod) totalGrids (nc ^. ncGridPerBlock)
    ic = InternalConfig
          { _icLengthPerNode = nc ^. ncLengthPerNode
          , _icGridPerNode = nc ^. ncGridPerNode
          , _icBlockPerNode = ms
          , _icGridPerBlock = nc ^. ncGridPerBlock
          , _icTemporalBlockingInterval = nc ^. ncTemporalBlockingInterval
          , _icSleeve = s
          , _icMPIShape = nc ^. ncMPIShape
          }
    check :: InternalConfig -> Either ConfigException InternalConfig
    check cfg | any (<1) (cfg ^. icLengthPerNode) = Left $ ConfigException "the element of length_per_node should be a positive number"
              | any (<1) (cfg ^. icGridPerNode) = Left $ ConfigException "the element of grid_per_node should be a positive integer"
              | any (<1) (cfg ^. icGridPerBlock) = Left $ ConfigException "the element of grid_per_block should be a positive integer"
              | any (<1) (cfg ^. icMPIShape) = Left $ ConfigException "the element of mpi_shape should be a positive integer"
              | any (/=0) ms' = Left $ ConfigException "Inconsistent config"
              | otherwise = Right cfg

nbuSize :: String -> InternalConfig -> Int
nbuSize a nc = 1 -- FIX ME
-- nbuSize a nc = head $ [ read $ drop (length kwd) opt
--                       | opt <- nc ^. ncOptionStrings
--                       , kwd `isPrefixOf` opt
--                       ] ++ [1]
--   where kwd = "nbu" ++ a

