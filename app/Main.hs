module Main where

import Control.Monad (forM, when)
import File
import SlocCLIParser (files, help, helpMessage, parseCommandLineArgs, recursive)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  cliArgs <- getArgs
  let runningOptions = parseCommandLineArgs cliArgs
  when (null (files runningOptions) && not (help runningOptions)) $ do
    putStrLn "Required argument: \'file/directory\' not set"
    putStrLn "Run with --help flag for more information"
    exitFailure
  when (help runningOptions) $ do
    putStrLn helpMessage
    exitSuccess
  sourceCodeFiles <- forM (files runningOptions) $ getSourceCodeFiles (recursive runningOptions)
  processedFiles <- forM (concat sourceCodeFiles) processFile
  print processedFiles
