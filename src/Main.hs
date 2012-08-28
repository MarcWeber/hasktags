{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where
import Hasktags
import Tags

import System.Environment

import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import Data.Maybe
import Data.Data
import Control.Monad( when )

import System.IO
import System.Environment
import System.Directory
import System.FilePath ((</>))
import System.Console.GetOpt
import System.Exit
import Text.JSON.Generic
import Control.Monad

options :: [OptDescr Mode]
options = [ Option "c" ["ctags"]
            (NoArg CTags) "generate CTAGS file (ctags)"
          , Option "e" ["etags"]
            (NoArg ETags) "generate ETAGS file (etags)"
          , Option "b" ["both"]
            (NoArg BothTags) "generate both CTAGS and ETAGS"
          , Option "a" ["append"]
            (NoArg Append) "append to existing CTAGS and/or ETAGS file(s). After this file will no longer be sorted!"
          , Option "" ["ignore-close-implementation"]
            (NoArg IgnoreCloseImpl) "ignores found implementation if its closer than 7 lines  - so you can jump to definition in one shot"
          , Option "o" ["output"]
            (ReqArg OutRedir "") "output to given file, instead of 'tags', '-' file is stdout"
          , Option "f" ["file"]
            (ReqArg OutRedir "") "same as -o, but used as compatibility with ctags"
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
                   "Usage: " ++ progName ++ " [OPTION...] [files or directories...]\n"
                ++ "directories will be replaced by DIR/**/*.hs DIR/**/*.lhs\n"
                ++ "Thus hasktags . tags all important files in the current directory"
        let (modes, files_or_dirs, errs) = getOpt Permute options args

        filenames <- liftM (nub . concat) $ mapM (dirToFiles False) files_or_dirs

        when (errs /= [] || elem Help modes || files_or_dirs == [])
             (do putStr $ unlines errs
                 putStr $ usageInfo usageString options
                 exitWith (ExitFailure 1))

        when (filenames == []) $ do
          putStrLn "warning: no files found!"

        let mode = getMode (filter ( `elem` [BothTags, CTags, ETags] ) modes)
            openFileMode = if elem Append modes
                           then AppendMode
                           else WriteMode
        filedata <- mapM (findWithCache (elem CacheFiles modes)
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
dirToFiles hsExtOnly p = do
  isD <- doesDirectoryExist p
  if isD then recurse p
         else return $ if not hsExtOnly || ".hs" `isSuffixOf` p || ".lhs" `isSuffixOf` p then [p] else []
  where recurse p = do
            names <- liftM (filter ( (/= '.') . head ) ) $ getDirectoryContents p
                                      -- skip . .. and hidden files (linux)
            liftM concat $ mapM (processFile . (p </>) ) names
        processFile f = dirToFiles True f
