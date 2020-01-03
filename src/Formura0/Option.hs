module Formura0.Option where

-- ^ コマンドラインオプション
data Option = Option
  { inputFilename  :: !FilePath
  , verbose        :: !Bool
  , configFilename :: !(Maybe FilePath)
  } deriving (Eq, Show)
