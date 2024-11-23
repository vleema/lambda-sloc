module FileType where

data FileType
  = Java FilePath
  | C FilePath
  | CPP FilePath
  | Rust FilePath
  | Header FileType
  | Regular
  deriving (Show, Eq)

instance Ord FileType where
  Header _ <= Rust _ = True
  C _ <= Header _ = True
  CPP _ <= C _ = True
  _ <= CPP _ = True
  _ <= _ = False

printFilePath :: FileType -> String
printFilePath = reverse . (++ ".../.") . reverse . getFilePath

printFileType :: FileType -> String
printFileType (Java _) = "Java"
printFileType (C _) = "C"
printFileType (CPP _) = "C++"
printFileType (Rust _) = "Rust"
printFileType (Header fileType) = printFilePath fileType ++ " Header"
printFileType Regular = ""

getFilePath :: FileType -> FilePath
getFilePath (Java path) = path
getFilePath (C path) = path
getFilePath (CPP path) = path
getFilePath (Rust path) = path
getFilePath (Header fileType) = getFilePath fileType
getFilePath Regular = ""
