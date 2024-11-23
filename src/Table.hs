module Table where

import Data.List (sortBy)
import Data.Ord (comparing)
import Text.Printf (printf)

import File (File (..))
import FileType (getFilePath, printFileType)
import SlocCLIParser (SortField (..), SortOrder (Ascending))

printTable :: (SortField, SortOrder) -> [File] -> IO ()
printTable _ [] = putStrLn "Nothing to be printed ¯\\_(ツ)_/¯"
printTable sortOpt files = do
  let sortedFiles = sortFiles sortOpt files
      processedFiles = "Files processed: " ++ show (length files)
      maxFileName = maximum (map (length . getFilePath . file) files)
      colWidths = [maxFileName, 10, 14, 14, 14, 10]
      separator = replicate (sum colWidths + 3 + length colWidths - 1) '-' -- This 3 is for the extra spaces between Filename and Language :P
      header = ["Filename", "Language", "Code", "Comments", "Blank", "All"]
      aligns = [True, True, False, False, False, False]
      formattedHeader = formatRow colWidths header aligns
      rows = map (formatFileRow colWidths) sortedFiles
      totalLoc = sum $ map loc sortedFiles
      totalComments = sum $ map comments sortedFiles
      totalBlank = sum $ map blank sortedFiles
      totalTotal = sum $ map total sortedFiles
      sumRow = formatSumRow colWidths totalLoc totalComments totalBlank totalTotal
  putStrLn processedFiles
  putStrLn separator
  putStrLn formattedHeader
  putStrLn separator
  mapM_ putStrLn rows
  putStrLn separator
  putStrLn sumRow
  putStrLn separator

padStringLeft :: Int -> String -> String
padStringLeft width str =
  if length str >= width
    then take width str
    else str ++ replicate (width - length str) ' '

padStringRight :: Int -> String -> String
padStringRight width str =
  if length str >= width
    then take width str
    else replicate (width - length str) ' ' ++ str

-- Formatting functions
formatRow :: [Int] -> [String] -> [Bool] -> String
formatRow colWidths cols aligns = concat $ interleave paddedCols spaces
 where
  pad width str alignLeft = if alignLeft then padStringLeft width str else padStringRight width str
  paddedCols = zipWith3 pad colWidths cols aligns
  spaces = "   " : replicate (length cols - 1) " "
  interleave (x : xs) (y : ys) = x : y : interleave xs ys
  interleave [x] [] = [x]
  interleave [] _ = []

formatFileRow :: [Int] -> File -> String
formatFileRow colWidths f =
  let filename = getFilePath (file f)
      language = printFileType (file f)
      codeLines = loc f
      commentLines = comments f
      blankLines = blank f
      totalLines = total f
      codePercent = if totalLines > 0 then (fromIntegral codeLines / fromIntegral totalLines) * 100 :: Double else 0
      commentPercent = if totalLines > 0 then (fromIntegral commentLines / fromIntegral totalLines) * 100 :: Double else 0
      blankPercent = if totalLines > 0 then (fromIntegral blankLines / fromIntegral totalLines) * 100 :: Double else 0
      codeStr = printf "%d (%.1f%%)" codeLines codePercent
      commentStr = printf "%d (%.1f%%)" commentLines commentPercent
      blankStr = printf "%d (%.1f%%)" blankLines blankPercent
      totalStr = show totalLines
      cols = [filename, language, codeStr, commentStr, blankStr, totalStr]
      aligns = [True, True, False, False, False, False]
   in formatRow colWidths cols aligns

formatSumRow :: [Int] -> Int -> Int -> Int -> Int -> String
formatSumRow colWidths totalLoc totalComments totalBlank totalTotal =
  let cols = ["SUM", "", show totalLoc, show totalComments, show totalBlank, show totalTotal]
      aligns = [True, True, False, False, False, False]
   in formatRow colWidths cols aligns

-- Main printTable function

sortFiles :: (SortField, SortOrder) -> [File] -> [File]
sortFiles (sField, Ascending) = sortFilesByField sField
sortFiles (sField, _) = reverse . sortFilesByField sField

sortFilesByField :: SortField -> [File] -> [File]
sortFilesByField Default = id
sortFilesByField Sloc = sortBy (comparing loc)
sortFilesByField Comments = sortBy (comparing comments)
sortFilesByField Filename = sortBy (comparing (getFilePath . file))
sortFilesByField All = sortBy (comparing total)
sortFilesByField Blanks = sortBy (comparing blank)
sortFilesByField Filetype = sortBy (comparing file)
