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
  let totalMPI = product $ ic ^. icMPIShape

  let cs = generate mm hxxFilePath cxxFilePath scaffold
      (hContent, cContent) = render cs

  let runningScriptPath = "run"
  genRunningScript runningScriptPath totalMPI

  writeFile hxxFilePath hContent
  writeFile cxxFilePath cContent
  putStr . unlines $ [ "Generate:"
                     , "  " ++ cxxFilePath
                     , "  " ++ hxxFilePath
                     , "  " ++ runningScriptPath
                     ]

genRunningScript :: String -> Int -> IO ()
genRunningScript fn n = do
  let script = unlines ["#!/bin/bash"
                       ,"prog=${1:?Need an executable file path}"
                       ,"opt=${2}"
                       ,"mpirun ${opt} -n " ++ show n ++ " ${prog}"
                       ]
  writeFile fn script
  p <- getPermissions fn
  setPermissions fn $ p {executable = True}
