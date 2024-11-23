module Main where

import Control.Monad (forM, when)
import File (getSourceCodeFiles, processFile)
import SlocCLIParser (Options, files, help, helpMessage, parseCommandLineArgs, recursive, runWithHelp)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

main :: IO ()
main = do
  cliArgs <- getArgs
  case parseCommandLineArgs cliArgs of
    Left runningOptions -> sloc runningOptions
    Right errorMsg -> putStrLn $ "Error: " ++ errorMsg ++ "\n" ++ runWithHelp

sloc :: Options -> IO ()
sloc runningOptions = do
  when (help runningOptions) $ do
    putStrLn helpMessage
    exitSuccess
  sourceCodeFiles <- forM (files runningOptions) $ getSourceCodeFiles (recursive runningOptions)
  processedFiles <- forM (concat sourceCodeFiles) processFile
  print processedFiles
