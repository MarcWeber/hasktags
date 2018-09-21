{-# LANGUAGE CPP #-}

#ifndef CURRENT_PACKAGE_VERSION
#define CURRENT_PACKAGE_VERSION "unknown"
#endif

module Main (main) where
import           Hasktags

import           System.Environment

import           Data.List

import           Control.Monad
import           System.Console.GetOpt
import           System.Directory
import           System.Exit
import Data.Version (showVersion)
import Paths_hasktags (version)

hsSuffixesDefault :: Mode
hsSuffixesDefault =  HsSuffixes [ ".hs", ".lhs", ".hsc" ]

options :: [OptDescr Mode]
options = [ Option "c" ["ctags"]
            (NoArg CTags) "generate CTAGS file (ctags)"
          , Option "e" ["etags"]
            (NoArg ETags) "generate ETAGS file (etags)"
          , Option "b" ["both"]
            (NoArg BothTags) "generate both CTAGS and ETAGS"
          , Option "a" ["append"]
              (NoArg Append)
            $ "append to existing CTAGS and/or ETAGS file(s). Afterward this "
              ++ "file will no longer be sorted!"
          , Option "o" ["output"]
            (ReqArg OutRedir "")
            "output to given file, instead of 'tags', '-' file is stdout"
          , Option "f" ["file"]
            (ReqArg OutRedir "")
            "same as -o, but used as compatibility with ctags"
          , Option "x" ["extendedctag"]
            (NoArg ExtendedCtag) "Generate additional information in ctag file."
          , Option "" ["cache"] (NoArg CacheFiles) "Cache file data."
          , Option "L" ["follow-symlinks"] (NoArg FollowDirectorySymLinks) "follow symlinks when recursing directories"
          , Option "S" ["suffixes"] (OptArg suffStr ".hs,.lhs") "list of hs suffixes including \".\""
          , Option "R" ["tags-absolute"] (NoArg AbsolutePath) "make tags paths absolute. Useful when setting tags files in other directories"
          , Option "h" ["help"] (NoArg Help) "This help"
          , Option "v" ["version"] (NoArg Version) "print the version of hasktags and exit"
          ]
  where suffStr Nothing  = hsSuffixesDefault
        suffStr (Just s) = HsSuffixes $ strToSuffixes s
        strToSuffixes = lines . map commaToEOL
        commaToEOL ',' = '\n'
        commaToEOL x   = x


main :: IO ()
main = do
        progName <- getProgName
        args <- getArgs
        let usageString =
                   "Usage: " ++ progName ++ " " ++ showVersion version
                ++ " [OPTION...] [files or directories...]\n"
                ++ "directories will be replaced by DIR/**/*.hs DIR/**/*.lhs\n"
                ++ "Thus hasktags . tags all important files in the current\n"
                ++ "directory.\n"
                ++ "\n"
                ++ "If directories are symlinks they will not be followed\n"
                ++ "unless you pass -L.\n"
                ++ "\n"
                ++ "A special file \"STDIN\" will make hasktags read the line separated file\n"
                ++ "list to be tagged from STDIN.\n"
        let (modes, files_or_dirs_unexpanded, errs) = getOpt Permute options args
#if debug
        print $ "modes: " ++ (show modes)
#endif

        files_or_dirs <- if AbsolutePath `elem` modes
                             then sequence $ map canonicalizePath files_or_dirs_unexpanded
                             else return files_or_dirs_unexpanded
        let hsSuffixes = head $ [ s | (HsSuffixes s) <- modes ++ [hsSuffixesDefault] ]

        let followSymLinks = FollowDirectorySymLinks `elem` modes

        filenames
          <- liftM (nub . concat) $ mapM (dirToFiles followSymLinks hsSuffixes) files_or_dirs

        when (elem Version modes)
            (do putStrLn CURRENT_PACKAGE_VERSION
                exitSuccess)

        when (errs /= [] || elem Help modes || files_or_dirs == [])
             (do putStr $ unlines errs
                 putStr $ usageInfo usageString options
                 exitWith (ExitFailure 1))

        when (filenames == []) $ putStrLn "warning: no files found!"

        generate modes filenames

-- Local Variables:
-- dante-target: "exe:hasktags"
-- End:
