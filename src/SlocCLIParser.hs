module SlocCLIParser where

data Options = Options
  { recursive :: Bool
  , files :: [FilePath]
  , help :: Bool
  }

helpMessage :: String
helpMessage =
  "Welcome to λsloc, there's nothing implemented yet, but will.\n\
  \\n\
  \USAGE:\n\
  \    ./sloc [OPTIONS] file/directory...\n\
  \\n\
  \OPTIONS:\n \
  \    -h/--help       Display this information.\n\
  \    -r              Look for files recursively in the directory provided.\n\
  \    -s f/t/c/b/s/a  Sort table in ascending order by (f)ilename, (t) filetype,\n\
  \                    (c)omments, (b)lank lines, (s)loc, or (a)ll. Default is to show\n\
  \                    files in ordem of appearance.\n\
  \    -S f/t/c/b/s/a  Same as above, but in descending order.\n"

parseCommandLineArgs :: [String] -> Options
parseCommandLineArgs args =
  if "-h" `elem` args || "--help" `elem` args
    then Options{recursive = False, files = [], help = True}
    else
      let recursiveFlag = "-r" `elem` args || "--recursive" `elem` args
          nonFlags = filter notFlags args
       in Options{recursive = recursiveFlag, files = nonFlags, help = False}
 where
  notFlags arg = arg /= "-r" && arg /= "--recursive"
