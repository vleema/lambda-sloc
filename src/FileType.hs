module FileType where

data FileType
  = Java FilePath
  | C FilePath
  | CPP FilePath
  | Rust FilePath
  | Header FileType
  | None
  deriving (Show, Eq)

printFilePath :: FileType -> String
printFilePath = reverse . (++ ".../.") . reverse . getFilePath

printFileType :: FileType -> String
printFileType (Java _) = "Java"
printFileType (C _) = "C"
printFileType (CPP _) = "C++"
printFileType (Rust _) = "Rust"
printFileType (Header fileType) = printFilePath fileType ++ " Header"
printFileType None = ""

getFilePath :: FileType -> FilePath
getFilePath (Java path) = path
getFilePath (C path) = path
getFilePath (CPP path) = path
getFilePath (Rust path) = path
getFilePath (Header fileType) = getFilePath fileType
getFilePath None = ""
