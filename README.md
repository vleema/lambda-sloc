# λsloc

This program intends to count the lines of code of multiple programming languages, such as C/C++, Java, Kotlin, Rust, JavaScript, Haskell, Python. `sloc` receive as input one or more files or directories, having the option to act recursively in sub-directories.

A sample output would be:

```terminal
$ ./sloc src/main.cpp
Files processed: 1
-----------------------------------------------------------------------------------------
Filename            Language        Comments        Blank           Code            All
-----------------------------------------------------------------------------------------
src/main.cpp        C++             140 (29.9%)     43 (9.2%)       320 (68.4%)     468
-----------------------------------------------------------------------------------------
```

## Usage

You can see program information with the `-h` or `--help` flag. The available options are:

```terminal
$ ./sloc -h
Welcome to λsloc, version 1.0.
Usage: sloc [options] file/directory
Options:
  -h/--help       Display this information.
  -r              Look for files recursively in the directory provided.
  -s f/t/c/b/s/a  Sort table in ascending order by (f)ilename, (t) filetype,
                  (c)omments, (b)lank lines, (s)loc, or (a)ll. Default is to show
                  files in ordem of appearance.
  -S f/t/c/b/s/a  Same as above, but in descending order.
```

#### Run and or Installing

The program is written in Haskell, and was packaged by `cabal`. So you can run it with `cabal run` or install it with `cabal install`.

```terminal
cabal run sloc -- <args>
cabal install sloc
```

```
