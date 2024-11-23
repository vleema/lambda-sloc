module Table where

import Data.List (sortBy)
import Data.Ord (comparing)

import File (File (..))
import FileType (getFilePath)
import SlocCLIParser (SortField (..), SortOrder)

printTable :: (SortField, SortOrder) -> [File] -> IO ()
printTable _ _ = putStrLn "Hi mom"

sortFiles :: SortField -> [File] -> [File]
sortFiles Default = id
sortFiles Sloc = sortBy (comparing loc)
sortFiles Comments = sortBy (comparing comments)
sortFiles Filename = sortBy (comparing (getFilePath . file))
sortFiles All = sortBy (comparing total)
sortFiles Blanks = sortBy (comparing blank)
sortFiles Filetype = sortBy (comparing file)
