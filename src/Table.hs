module Table where

import Data.List (sortBy)
import Data.Ord (comparing)

import File (File (..))
import FileType (getFilePath)

sortByLoc :: [File] -> [File]
sortByLoc = sortBy (comparing loc)

sortByComments :: [File] -> [File]
sortByComments = sortBy (comparing comments)

sortByFileName :: [File] -> [File]
sortByFileName = sortBy (comparing (getFilePath . file))

sortByTotalLines :: [File] -> [File]
sortByTotalLines = sortBy (comparing total)

sortByBlankLines :: [File] -> [File]
sortByBlankLines = sortBy (comparing blank)
