module FileType where

import Data.List (isSuffixOf)

data FileType
  = Java FilePath
  | C FilePath
  | CPP FilePath
  | Rust FilePath
  | Go FilePath
  | Header FileType
  | Regular
  deriving (Show, Eq)

instance Ord FileType where
  Header _ <= Rust _ = True
  C _ <= Header _ = True
  CPP _ <= C _ = True
  Go _ <= CPP _ = True
  _ <= Go _ = True
  _ <= _ = False

sourceCodeFile :: FilePath -> FileType
sourceCodeFile file_
  | ".java" `isSuffixOf` file_ = Java file_
  | ".c" `isSuffixOf` file_ = C file_
  | ".cpp" `isSuffixOf` file_ = CPP file_
  | ".h" `isSuffixOf` file_ = Header (C file_)
  | ".hpp" `isSuffixOf` file_ = Header (CPP file_)
  | ".rs" `isSuffixOf` file_ = Rust file_
  | ".go" `isSuffixOf` file_ = Go file_
  | otherwise = Regular

printFileType :: FileType -> String
printFileType (Java _) = "Java"
printFileType (C _) = "C"
printFileType (CPP _) = "C++"
printFileType (Rust _) = "Rust"
printFileType (Go _) = "Go"
printFileType (Header fileType) = printFileType fileType ++ " Header"
printFileType Regular = ""

getFilePath :: FileType -> FilePath
getFilePath (Java path) = path
getFilePath (C path) = path
getFilePath (CPP path) = path
getFilePath (Rust path) = path
getFilePath (Go path) = path
getFilePath (Header fileType) = getFilePath fileType
getFilePath Regular = ""
