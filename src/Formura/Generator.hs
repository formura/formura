{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Formura.Generator where

import Control.Lens hiding (op)
import System.Directory

import Formura.CommandLineOption
import Formura.GlobalEnvironment
import Formura.Generator.Encode
import Formura.Generator.Types
import Formura.Generator.Templates
import Formura.NumericalConfig
import Formura.OrthotopeMachine.Graph

genCode :: WithCommandLineOption => MMProgram -> IO ()
genCode mm = do
  let ic = mm ^. omGlobalEnvironment . envNumericalConfig
  let totalMPI = product <$> ic ^. icMPIShape

  let cs = genCodeStructure mm scaffold
      (hContent, cContent) = render hxxFilePath cs

  let runningScriptPath = "run"
  genRunningScript runningScriptPath totalMPI

  writeFile hxxFilePath hContent
  writeFile cxxFilePath cContent
  putStr . unlines $ [ "Generate:"
                     , "  " ++ cxxFilePath
                     , "  " ++ hxxFilePath
                     , "  " ++ runningScriptPath
                     ]

genRunningScript :: String -> Maybe Int -> IO ()
genRunningScript fn mn = do
  let script = unlines $ case mn of
        Nothing -> ["#!/bin/bash"
                   ,"prog=${1:?Need an executable file path}"
                   ,"${prog}"
                   ]
        Just n -> ["#!/bin/bash"
                  ,"prog=${1:?Need an executable file path}"
                  ,"opt=${2}"
                  ,"mpirun ${opt} -n " ++ show n ++ " ${prog}"
                  ]
  writeFile fn script
  p <- getPermissions fn
  setPermissions fn $ p {executable = True}
