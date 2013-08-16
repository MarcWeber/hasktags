{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where
import Hasktags
import Tags

import System.Environment

import Data.List

import System.IO
import System.Directory
#ifdef VERSION_unix
import System.Posix.Files
#endif
import System.FilePath ((</>))
import System.Console.GetOpt
import System.Exit
import Control.Monad

options :: [OptDescr Mode]
options = [ Option "c" ["ctags"]
            (NoArg CTags) "generate CTAGS file (ctags)"
          , Option "e" ["etags"]
            (NoArg ETags) "generate ETAGS file (etags)"
          , Option "b" ["both"]
            (NoArg BothTags) "generate both CTAGS and ETAGS"
          , Option "a" ["append"]
              (NoArg Append)
            $ "append to existing CTAGS and/or ETAGS file(s). After this file "
              ++ "will no longer be sorted!"
          , Option "" ["ignore-close-implementation"]
              (NoArg IgnoreCloseImpl)
            $ "ignores found implementation if its closer than 7 lines  - so "
              ++ "you can jump to definition in one shot"
          , Option "o" ["output"]
            (ReqArg OutRedir "")
            "output to given file, instead of 'tags', '-' file is stdout"
          , Option "f" ["file"]
            (ReqArg OutRedir "")
            "same as -o, but used as compatibility with ctags"
          , Option "x" ["extendedctag"]
            (NoArg ExtendedCtag) "Generate additional information in ctag file."
          , Option "" ["cache"] (NoArg CacheFiles) "Cache file data."
          , Option "h" ["help"] (NoArg Help) "This help"
          ]


main :: IO ()
main = do
        progName <- getProgName
        args <- getArgs
        let usageString =
                   "Usage: " ++ progName
                ++ " [OPTION...] [files or directories...]\n"
                ++ "directories will be replaced by DIR/**/*.hs DIR/**/*.lhs\n"
                ++ "Thus hasktags . tags all important files in the current "
                ++ "directory"
        let (modes, files_or_dirs, errs) = getOpt Permute options args

        filenames
          <- liftM (nub . concat) $ mapM (dirToFiles True) files_or_dirs

        when (errs /= [] || elem Help modes || files_or_dirs == [])
             (do putStr $ unlines errs
                 putStr $ usageInfo usageString options
                 exitWith (ExitFailure 1))

        when (filenames == []) $ putStrLn "warning: no files found!"

        let mode = getMode (filter ( `elem` [BothTags, CTags, ETags] ) modes)
            openFileMode = if Append `elem` modes
                           then AppendMode
                           else WriteMode
        filedata <- mapM (findWithCache (CacheFiles `elem` modes)
                                        (IgnoreCloseImpl `elem` modes))
                         filenames

        when (mode == CTags)
             (do ctagsfile <- getOutFile "tags" openFileMode modes
                 writectagsfile ctagsfile (ExtendedCtag `elem` modes) filedata
                 hClose ctagsfile)

        when (mode == ETags)
             (do etagsfile <- getOutFile "TAGS" openFileMode modes
                 writeetagsfile etagsfile filedata
                 hClose etagsfile)

        -- avoid problem when both is used in combination
        -- with redirection on stdout
        when (mode == BothTags)
             (do etagsfile <- getOutFile "TAGS" openFileMode modes
                 writeetagsfile etagsfile filedata
                 ctagsfile <- getOutFile "tags" openFileMode modes
                 writectagsfile ctagsfile (ExtendedCtag `elem` modes) filedata
                 hClose etagsfile
                 hClose ctagsfile)

dirToFiles :: Bool -> FilePath -> IO [ FilePath ]
dirToFiles named p = do
  isD <- doesDirectoryExist p
  case isD of
    False -> return $ if named || isHaskell then [p] else []
    True -> do
#ifdef VERSION_unix
      isL <- isSymbolicLink `fmap` getSymbolicLinkStatus p
      if not named && isL then return [] else do
#endif
        contents <- filter ((/=) "." . take 1) `fmap` getDirectoryContents p
        concat `fmap` mapM (dirToFiles False . (</>) p) contents
  where isHaskell = any (`isSuffixOf` p) [".hs", ".lhs"]
