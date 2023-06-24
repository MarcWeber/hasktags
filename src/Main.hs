{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where
import Hasktags

import Control.Monad (unless)
import Data.Monoid
import Data.Set (Set, notMember, fromList, union)
import Data.Version (showVersion)
import Options.Applicative
import Options.Applicative.Help.Pretty (pretty, line)
import Paths_hasktags (version)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (IOMode (AppendMode, WriteMode))

import qualified Data.Set as Set

data Options = Options
  { _mode :: Mode
  , _optionFiles :: [FilePath]
  , _files :: [FilePath]
  } deriving Show

options :: Parser Options
options = Options
    <$> mode
    <*> many optionFiles
    <*> files
  where
    mode :: Parser Mode
    mode = Mode
      <$> (ctags <|> etags <|> bothTags)
      <*> extendedCtag
      <*> appendTags
      <*> outputRedirection
      <*> cacheData
      <*> followSymlinks
      <*> suffixes
      <*> absoluteTagPaths
    ctags :: Parser Tags
    ctags = flag Both Ctags $
         long "ctags"
      <> short 'c'
      <> help "generate CTAGS file (ctags)"

    etags :: Parser Tags
    etags = flag Both Etags  $
         long "etags"
      <> short 'e'
      <> help "generate ETAGS file (etags)"

    bothTags :: Parser Tags
    bothTags = flag' Both $
         long "both"
      <> short 'b'
      <> help "generate both CTAGS and ETAGS (default)"

    extendedCtag :: Parser Bool
    extendedCtag = switch $
         long "extendedctag"
      <> short 'x'
      <> showDefault
      <> help "Generate additional information in ctag file."

    appendTags :: Parser IOMode
    appendTags = flag WriteMode AppendMode $
         long "append"
      <> short 'a'
      <> showDefault
      <> help "append to existing CTAGS and/or ETAGS file(s). Afterward this file will no longer be sorted!"

    outputRedirection :: Parser TagsFile
    outputRedirection = strOption $
         long "output"
      <> long "file"
      <> short 'o'
      <> short 'f'
      <> metavar "FILE|-"
      <> value (TagsFile "tags" "TAGS")
      <> showDefault
      <> help "output to given file, instead of using the default names. '-' writes to stdout"

    cacheData :: Parser Bool
    cacheData = switch $
         long "cache"
      <> showDefault
      <> help "cache file data"

    followSymlinks :: Parser Bool
    followSymlinks = switch $
         long "follow-symlinks"
      <> short 'L'
      <> showDefault
      <> help "follow symlinks when recursing directories"

    suffixes :: Parser [String]
    suffixes = option auto $
         long "suffixes"
      <> short 'S'
      <> value [".hs", ".lhs", ".hsc"]
      <> showDefault
      <> help "list of hs suffixes including \".\""

    absoluteTagPaths :: Parser Bool
    absoluteTagPaths = switch $
         long "tags-absolute"
      <> short 'R'
      <> showDefault
      <> help "make tags paths absolute. Useful when setting tags files in other directories"

    files :: Parser [FilePath]
    files = some $ argument str (metavar "<files or directories...>")

    optionFiles :: Parser FilePath
    optionFiles = strOption $
         long "options"
      <> metavar "FILE"
      <> help "read additional options from file. The file should contain one option per line"

type Argument = String

parseArgs :: [Argument] -> Set FilePath -> IO Options
parseArgs args parsedOptionFiles = do
  parsedOptions@Options{..} <- handleParseResult $ execParserPure defaultPrefs opts args

  let filesToParse = nonParsedFiles _optionFiles

  if null filesToParse
    then return parsedOptions
    else do
      mapM_ dieIfFilesDoesntExist filesToParse
      newFlags <- parseArgsFromFiles filesToParse
      parseArgs (args ++ newFlags) (fromList filesToParse `union` parsedOptionFiles)

  where
    dieIfFilesDoesntExist :: FilePath -> IO ()
    dieIfFilesDoesntExist file = do
          exists <- doesFileExist file
          unless exists (die $ file ++ " from --options doesn't exist")

    nonParsedFiles :: [FilePath] -> [FilePath]
    nonParsedFiles = filter (`notMember` parsedOptionFiles)

    parseArgsFromFiles :: [FilePath] -> IO [Argument]
    parseArgsFromFiles fps = concat <$> mapM parseArgsFromFile fps
      where
        parseArgsFromFile :: FilePath -> IO [Argument]
        parseArgsFromFile fp = lines <$> readFile fp

    opts = info (options <**> versionFlag <**> helper) $
         fullDesc
      <> progDescDoc (Just $
             replaceDirsInfo <> line <> line
          <> symlinksInfo <> line <> line
          <> stdinInfo)
      where
        versionFlag = infoOption (showVersion version) $
             long "version"
          <> help "show version"

        replaceDirsInfo = pretty $ unwords
          [
            "directories will be replaced by DIR/**/*.hs DIR/**/*.lhs"
          , "Thus hasktags . tags all important files in the current directory."
          ]
        symlinksInfo = pretty $ unwords
          [
            "If directories are symlinks they will not be followed"
          , "unless you pass -L."
          ]
        stdinInfo = pretty $ unwords
          [
            "A special file \"STDIN\" will make hasktags read the line separated file "
          , "list to be tagged from STDIN."
          ]

main :: IO ()
main = do
  args <- getArgs
  Options{..} <- parseArgs args Set.empty

  generate _mode _files
