module SlocCLIParser where

helpMessage :: String
helpMessage =
  "Welcome to λsloc, there's nothing implemented yet, but will.\n\
  \\n\
  \USAGE:\n\
  \    ./sloc [OPTIONS]... file/directory...\n\
  \\n\
  \OPTIONS:\n \
  \    -h/--help       Display this information.\n\
  \    -r              Look for files recursively in the directory provided.\n\
  \    -s f/t/c/b/s/a  Sort table in ascending order by (f)ilename, (t) filetype,\n\
  \                    (c)omments, (b)lank lines, (s)loc, or (a)ll. Default is to show\n\
  \                    files in ordem of appearance.\n\
  \    -S f/t/c/b/s/a  Same as above, but in descending order.\n"

runWithHelp :: String
runWithHelp = "Run with --help flag for more information"

data Options = Options
  { recursive :: Bool
  , files :: [String]
  , help :: Bool
  , sort :: (SortField, SortOrder)
  }
  deriving (Show)

data SortField = Filename | Filetype | Comments | Blanks | Sloc | All | Default
  deriving (Eq, Show)

data SortOrder = Ascending | Descending | None
  deriving (Eq, Show)

type ErrorMessage = String

parseCommandLineArgs :: [String] -> Either Options ErrorMessage
parseCommandLineArgs args = processArgs args defaultOptions
 where
  defaultOptions =
    Options
      { recursive = False
      , files = []
      , help = False
      , sort = (Default, None)
      }

  processArgs :: [String] -> Options -> Either Options ErrorMessage
  processArgs [] opts = if null (files opts) && not (help opts) then Right "Required argument: \'file/directory\' not set" else Left opts
  -- Help
  processArgs ("-h" : rest) opts
    | help opts = Right "Option -h/--help specified multiple times"
    | otherwise = processArgs rest opts{help = True}
  processArgs ("--help" : rest) opts
    | help opts = Right "Option -h/--help specified multiple times"
    | otherwise = processArgs rest opts{help = True}
  -- Flags
  processArgs ("-r" : rest) opts
    | recursive opts = Right "Option -r specified multiple time"
    | otherwise = processArgs rest opts{recursive = True}
  -- Sorting options
  processArgs ("-s" : sField : rest) opts
    | snd (sort opts) /= None = Right "Sort option specified multiple times"
    | otherwise =
        case parseSortField sField of
          Just field -> processArgs rest opts{sort = (field, Ascending)}
          Nothing -> Right $ "Invalid sort field after -s: " ++ sField
  processArgs ["-s"] _ = Right "Missing sort field after -s"
  processArgs ("-S" : sField : rest) opts
    | snd (sort opts) /= None = Right "Sort option specified multiple times"
    | otherwise =
        case parseSortField sField of
          Just field -> processArgs rest opts{sort = (field, Descending)}
          Nothing -> Right $ "Invalid sort field after -S: " ++ sField
  processArgs ["-S"] _ = Right "Missing sort field after -S"
  -- Files and Unknown options
  processArgs (arg@('-' : _) : _) _ = Right $ "Unknown option: " ++ arg
  processArgs (file : rest) opts = processArgs rest opts{files = files opts ++ [file]}

  parseSortField :: String -> Maybe SortField
  parseSortField "f" = Just Filename
  parseSortField "t" = Just Filetype
  parseSortField "c" = Just Comments
  parseSortField "b" = Just Blanks
  parseSortField "s" = Just Sloc
  parseSortField "a" = Just All
  parseSortField _ = Nothing
