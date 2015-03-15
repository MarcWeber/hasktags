{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
-- should this be named Data.Hasktags or such?
module Hasktags (
  FileData,
  generate,
  findWithCache,
  findThings,
  findThingsInBS,

  Mode(..),
  --  TODO think about these: Must they be exported ?
  getMode,
  getOutFile,

  dirToFiles
) where

import Tags

-- the lib
import qualified Data.ByteString.Char8 as BS

import qualified Data.ByteString.UTF8 as BS8 -- see notes at utf8_to_char8_hack

import Data.Char
import Data.List
import Data.Maybe

import System.IO
import System.Directory
import System.FilePath (takeFileName)
import Text.JSON.Generic
import Control.Monad

import DebugShow

#ifdef VERSION_unix
import System.Posix.Files
#endif
import System.FilePath ((</>))

-- search for definitions of things
-- we do this by looking for the following patterns:
-- data XXX = ...      giving a datatype location
-- newtype XXX = ...   giving a newtype location
-- bla :: ...          giving a function location
--
-- by doing it this way, we avoid picking up local definitions
--              (whether this is good or not is a matter for debate)
--

-- We generate both CTAGS and ETAGS format tags files
-- The former is for use in most sensible editors, while EMACS uses ETAGS

-- alternatives: http://haskell.org/haskellwiki/Tags

{- .hs or literate .lhs haskell file?
Really not a easy question - maybe there is an answer - I don't know

.hs -> non literate haskel file
.lhs -> literate haskell file
.chs -> is this always plain?
.whatsoever -> try to get to know the answer (*)
  contains any '> ... ' line -> interpreted as literate
  else non literate

(*)  This is difficult because
 System.Log.Logger is using
  {-
  [...]
  > module Example where
  > [...]
  -}
  module System.Log.Logger(
  so it might looks like beeing a .lhs file
  My first fix was checking for \\begin occurence (doesn't work because HUnit is
  using > but no \\begin)
  Further ideas:
    * use unlit executable distributed with ghc or the like and check for
      errors?
      (Will this work if cpp is used as well ?)
    * Remove comments before checking for '> ..'
      does'nt work because {- -} may be unbalanced in literate comments
  So my solution is : take file extension and keep guessing code for all unkown
  files
-}


-- Reference: http://ctags.sourceforge.net/FORMAT


-- | getMode takes a list of modes and extracts the mode with the
--   highest precedence.  These are as follows: Both, CTags, ETags
--   The default case is Both.
getMode :: [Mode] -> Mode
getMode [] = BothTags
getMode xs = maximum xs

-- | getOutFile scans the modes searching for output redirection
--   if not found, open the file with name passed as parameter.
--   Handle special file -, which is stdout
getOutFile :: String -> IOMode -> [Mode] -> IO Handle
getOutFile _           _        (OutRedir "-" : _) = return stdout
getOutFile _           openMode (OutRedir f : _)   = openFile f openMode
getOutFile name        openMode (_:xs)             = getOutFile name openMode xs
getOutFile defaultName openMode []                 = openFile
                                                     defaultName
                                                     openMode

data Mode = ExtendedCtag
          | IgnoreCloseImpl
          | ETags
          | CTags
          | BothTags
          | Append
          | OutRedir String
          | CacheFiles
          | FollowDirectorySymLinks
          | Help
          | HsSuffixes [String]
          | AbsolutePath
          | FileTags
          deriving (Ord, Eq, Show)

data Token = Token String Pos
            | NewLine Int -- space 8*" " = "\t"
  deriving (Eq)
instance Show Token where
  -- show (Token t (Pos _ l _ _) ) = "Token " ++ t ++ " " ++ (show l)
  show (Token t (Pos _ _l _ _) ) = " " ++ t ++ " "
  show (NewLine i) = "NewLine " ++ show i

tokenString :: Token -> String
tokenString (Token s _) = s
tokenString (NewLine _) = "\n"

isNewLine :: Maybe Int -> Token -> Bool
isNewLine Nothing (NewLine _) = True
isNewLine (Just c) (NewLine c') = c == c'
isNewLine _ _ = False

trimNewlines :: [Token] -> [Token]
trimNewlines = filter (not . isNewLine Nothing)

generate :: [Mode] -> [FileName] -> IO ()
generate modes filenames = do

  let mode = getMode (filter ( `elem` [BothTags, CTags, ETags] ) modes)
      openFileMode = if Append `elem` modes
                     then AppendMode
                     else WriteMode
  filedata <- mapM (findWithCache (CacheFiles `elem` modes)
                                  (IgnoreCloseImpl `elem` modes)
                                  (FileTags `elem` modes))
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

-- Append file name in the list of tags per file
appendFileTag :: FileData -> FileData
appendFileTag (FileData filename things) = FileData filename (tag:things)
  where tag = FoundThing FTFileName (takeFileName filename) pos
        pos = Pos filename 0 0 ""

-- Find the definitions in a file, or load from cache if the file
-- hasn't changed since last time.
findWithCache :: Bool -> Bool -> Bool -> FileName -> IO FileData
findWithCache cache ignoreCloseImpl fileTags filename = do
  cacheExists <- if cache then doesFileExist cacheFilename else return False
  if cacheExists
     then do fileModified <- getModificationTime filename
             cacheModified <- getModificationTime cacheFilename
             if cacheModified > fileModified
              then do bytes <- BS.readFile cacheFilename
                      return (decodeJSON (BS.unpack bytes))
              else findAndCache
     else findAndCache

  where cacheFilename = filenameToTagsName filename
        filenameToTagsName = (++"tags") . reverse . dropWhile (/='.') . reverse
        findAndCache = do
          filedata <- findThings ignoreCloseImpl filename
          let alldata = if fileTags then appendFileTag filedata else filedata
          when cache (writeFile cacheFilename (encodeJSON alldata))
          return alldata

-- eg Data.Text says that using ByteStrings could be fastest depending on ghc
-- platform and whatnot - so let's keep the hacky BS.readFile >>= BS.unpack
-- usage till there is a problem, still need to match utf-8 chars like this: ⇒
-- to get correct class names, eg MonadBaseControl case (testcase testcases/monad-base-control.hs)
-- so use the same conversion which is applied to files when they got read ..
utf8_to_char8_hack :: String -> String
utf8_to_char8_hack = BS.unpack . BS8.fromString

-- Find the definitions in a file
findThings :: Bool -> FileName -> IO FileData
findThings ignoreCloseImpl filename =
  fmap (findThingsInBS ignoreCloseImpl filename) $ BS.readFile filename

findThingsInBS :: Bool -> String -> BS.ByteString -> FileData
findThingsInBS ignoreCloseImpl filename bs = do
        let aslines = lines $ BS.unpack bs

        let stripNonHaskellLines = let
                  emptyLine = all (all isSpace . tokenString)
                            . filter (not . isNewLine Nothing)
                  cppLine (_nl:t:_) = ("#" `isPrefixOf`) $ tokenString t
                  cppLine _ = False
                in filter (not . emptyLine) . filter (not . cppLine)

        let debugStep m = (\s -> trace_ (m ++ " result") s s)

        let (isLiterate, slines) =
              debugStep "fromLiterate"
              $ fromLiterate filename
              $ zip aslines [0..]

        --  remove -- comments, then break each line into tokens (adding line
        --  numbers)
        --  then remove {- -} comments
        --  split by lines again ( to get indent
        let
          (fileLines, numbers)
            = unzip slines

        let tokenLines {- :: [[Token]] -} =
                        debugStep "stripNonHaskellLines" $ stripNonHaskellLines
                      $ debugStep "stripslcomments" $ stripslcomments
                      $ debugStep "splitByNL" $ splitByNL Nothing
                      $ debugStep "stripblockcomments pipe" $ stripblockcomments
                      $ concat
                      $ zipWith3 (withline filename)
                                 (map
                                   (filter (not . all isSpace) . mywords False)
                                   fileLines)
                                 fileLines
                                 numbers


        -- TODO  ($defines / empty lines etc)
        -- separate by top level declarations (everything starting with the
        -- same topmost indentation is what I call section here)
        -- so that z in
        -- let x = 7
        --     z = 20
        -- won't be found as function
        let topLevelIndent = debugStep "top level indent" $ getTopLevelIndent isLiterate tokenLines
        let sections = map tail -- strip leading NL (no longer needed
                       $ filter (not . null)
                       $ splitByNL (Just (topLevelIndent) )
                       $ concat (trace_ "tokenLines" tokenLines tokenLines)
        -- only take one of
        -- a 'x' = 7
        -- a _ = 0
        let filterAdjacentFuncImpl = nubBy (\(FoundThing t1 n1 (Pos f1 _ _ _))
                                             (FoundThing t2 n2 (Pos f2 _ _ _))
                                             -> f1 == f2
                                               && n1 == n2
                                               && t1 == FTFuncImpl
                                               && t2 == FTFuncImpl )

        let iCI = if ignoreCloseImpl
              then nubBy (\(FoundThing _ n1 (Pos f1 l1 _ _))
                         (FoundThing _ n2 (Pos f2 l2 _ _))
                         -> f1 == f2
                           && n1 == n2
                           && ((<= 7) $ abs $ l2 - l1))
              else id
        let things = iCI $ filterAdjacentFuncImpl $ concatMap findstuff $ map (\s -> trace_ "section in findThingsInBS" s s) sections
        let
          -- If there's a module with the same name of another definition, we
          -- are not interested in the module, but only in the definition.
          uniqueModuleName (FoundThing FTModule moduleName _)
            = not
              $ any (\(FoundThing thingType thingName _)
                -> thingType /= FTModule && thingName == moduleName) things
          uniqueModuleName _ = True
        FileData filename $ filter uniqueModuleName things

-- Create tokens from words, by recording their line number
-- and which token they are through that line

withline :: FileName -> [String] -> String -> Int -> [Token]
withline filename sourceWords fullline i =
  let countSpaces (' ':xs) = 1 + countSpaces xs
      countSpaces ('\t':xs) = 8 + countSpaces xs
      countSpaces _ = 0
  in NewLine (countSpaces fullline)
      : zipWith (\w t -> Token w (Pos filename i t fullline)) sourceWords [1 ..]

-- comments stripping

stripslcomments :: [[Token]] -> [[Token]]
stripslcomments = let f (NewLine _ : Token "--" _ : _) = False
                      f _ = True
                  in filter f

stripblockcomments :: [Token] -> [Token]
stripblockcomments (Token "{-" pos : xs) =
  trace_ "{- found at " (show pos) $
  afterblockcomend xs
stripblockcomments (x:xs) = x:stripblockcomments xs
stripblockcomments [] = []

afterblockcomend :: [Token] -> [Token]
afterblockcomend (t@(Token _ pos):xs)
 | contains "-}" (tokenString t) =
   trace_ "-} found at " (show pos) $
   stripblockcomments xs
 | otherwise           = afterblockcomend xs
afterblockcomend [] = []
afterblockcomend (_:xs) = afterblockcomend xs


-- does one string contain another string

contains :: Eq a => [a] -> [a] -> Bool
contains sub = any (isPrefixOf sub) . tails

-- actually pick up definitions

findstuff :: [Token] -> [FoundThing]
findstuff (Token "module" _ : Token name pos : _) =
        trace_ "module" pos $
        [FoundThing FTModule name pos] -- nothing will follow this section
findstuff tokens@(Token "data" _ : Token name pos : xs)
        | any ( (== "where"). tokenString ) xs -- GADT
            -- TODO will be found as FTCons (not FTConsGADT), the same for
            -- functions - but they are found :)
            =
              trace_  "findstuff data b1" tokens $
              FoundThing FTDataGADT name pos
              : getcons2 xs ++ fromWhereOn xs -- ++ (findstuff xs)
        | otherwise
            =
              trace_  "findstuff data otherwise" tokens $
              FoundThing FTData name pos
              : getcons FTData (trimNewlines xs)-- ++ (findstuff xs)
findstuff tokens@(Token "newtype" _ : ts@(Token name pos : _)) =
        trace_ "findstuff newtype" tokens $
        FoundThing FTNewtype name pos
          : getcons FTCons (trimNewlines ts)-- ++ (findstuff xs)
        -- FoundThing FTNewtype name pos : findstuff xs
findstuff tokens@(Token "type" _ : Token name pos : xs) =
        trace_  "findstuff type" tokens $
        FoundThing FTType name pos : findstuff xs
findstuff tokens@(Token "class" _ : xs) =
        trace_  "findstuff class" tokens $
        case (break ((== "where").tokenString) xs) of
        (ys, []) ->
          trace_ "findstuff class b1 " ys $
          maybeToList $ className ys
        (ys, r) ->
          trace_ "findstuff class b2 " (ys, r) $
             (maybeToList $ className ys)
          ++ (maybe [] (:fromWhereOn r) $ className xs)
    where isParenOpen (Token "(" _) = True
          isParenOpen _ = False
          className lst
            = case (head
                  . dropWhile isParenOpen
                  . reverse
                  . takeWhile ((not . (`elem` ["=>", utf8_to_char8_hack "⇒"])) . tokenString)
                  . reverse) lst of
              (Token name p) -> Just $ FoundThing FTClass name p
              _ -> Nothing
findstuff xs =
  trace_ "findstuff rest " xs $
  findFunc xs ++ findFuncTypeDefs [] xs

findFuncTypeDefs :: [Token] -> [Token] -> [FoundThing]
findFuncTypeDefs found (t@(Token _ _): Token "," _ :xs) =
          findFuncTypeDefs (t : found) xs
findFuncTypeDefs found (t@(Token _ _): Token "::" _ :_) =
          map (\(Token name p) -> FoundThing FTFuncTypeDef name p) (t:found)
findFuncTypeDefs found (Token "(" _ :xs) =
          case break myBreakF xs of
            (inner@(Token _ p : _), _:xs') ->
              let merged = Token ( concatMap (\(Token x _) -> x) inner ) p
              in findFuncTypeDefs found $ merged : xs'
            _ -> []
    where myBreakF (Token ")" _) = True
          myBreakF _ = False
findFuncTypeDefs _ _ = []

fromWhereOn :: [Token] -> [FoundThing]
fromWhereOn [] = []
fromWhereOn [_] = []
fromWhereOn (_: xs@(NewLine _ : _)) =
             concatMap (findstuff . tail')
             $ splitByNL (Just ( minimum
                                . (10000:)
                                . map (\(NewLine i) -> i)
                                . filter (isNewLine Nothing) $ xs)) xs
fromWhereOn (_:xw) = findstuff xw

findFunc :: [Token] -> [FoundThing]
findFunc x = case findInfix x of
    a@(_:_) -> a
    _ -> findF x

findInfix :: [Token] -> [FoundThing]
findInfix x
   = case dropWhile
       ((/= "`"). tokenString)
       (takeWhile ( (/= "=") . tokenString) x) of
     _ : Token name p : _ -> [FoundThing FTFuncImpl name p]
     _ -> []


findF :: [Token] -> [FoundThing]
findF (Token name p : xs) =
    [FoundThing FTFuncImpl name p | any (("=" ==) . tokenString) xs]
findF _ = []

tail' :: [a] -> [a]
tail' (_:xs) = xs
tail' [] = []

-- get the constructor definitions, knowing that a datatype has just started

getcons :: FoundThingType -> [Token] -> [FoundThing]
getcons ftt (Token "=" _: Token name pos : xs) =
        FoundThing ftt name pos : getcons2 xs
getcons ftt (_:xs) = getcons ftt xs
getcons _ [] = []


getcons2 :: [Token] -> [FoundThing]
getcons2 (Token name pos : Token "::" _ : xs) =
        FoundThing FTConsAccessor name pos : getcons2 xs
getcons2 (Token "=" _ : _) = []
getcons2 (Token "|" _ : Token name pos : xs) =
        FoundThing FTCons name pos : getcons2 xs
getcons2 (_:xs) = getcons2 xs
getcons2 [] = []


splitByNL :: Maybe Int -> [Token] -> [[Token]]
splitByNL maybeIndent (nl@(NewLine _):ts) =
  let (a,b) = break (isNewLine maybeIndent) ts
  in (nl : a) : splitByNL maybeIndent b
splitByNL _ _ = []

-- this only exists for test case testcases/HUnitBase.lhs (bird literate haskell style)
getTopLevelIndent :: Bool -> [[Token]] -> Int
getTopLevelIndent _ [] = 0 -- (no import found, assuming indent 0: this can be
                           -- done better but should suffice for most needs
getTopLevelIndent isLiterate ((nl:next:_):xs) = if "import" == (tokenString next)
                          then let (NewLine i) = nl in i
                          else getTopLevelIndent isLiterate xs
getTopLevelIndent isLiterate (_:xs) = getTopLevelIndent isLiterate xs

-- According to http://www.haskell.org/onlinereport/literate.html either
-- birdstyle or LaTeX style should be used. However simple experiments show
-- that unlit distributed by GHC has the following behavior
-- * The space after ">" can be omitted
-- * ">" must be first char in line to be read as birdstyle (then its replaced by a space)
-- * \begin{code} gets recognized if its indented, but \end{code} does not (?)
--
-- Attention: Base.lhs (shipping with GHC) have birdstyle in block comments
fromLiterate :: FilePath -> [(String, Int)]
    -> (Bool -- is literate
    , [(String, Int)])
fromLiterate file lns =
  if ".lhs" `isSuffixOf` file
    then (True, unlit lns)
    else (False, lns)

  where unlit, returnCode :: [(String, Int)] -> [(String, Int)]
        unlit ((('>':' ':xs),n):ns) = ((' ':xs),n):unlit(ns) -- unlit keeps space, so do we
        unlit ((line,_):ns) = if "\\begin{code}" `isPrefixOf` line then returnCode ns else unlit ns
        unlit [] = []

        -- in \begin{code} block
        returnCode (t@(line,_):ns) = if "\\end{code}" `isPrefixOf` line then unlit ns else t:(returnCode ns)
        returnCode [] = [] -- unexpected - hasktags does tagging, not compiling, thus don't treat missing \end{code} to be an error

-- suffixes: [".hs",".lhs"], use "" to match all files
dirToFiles :: Bool -> [String] -> FilePath -> IO [ FilePath ]
dirToFiles _ _ "STDIN" = fmap lines $ hGetContents stdin
dirToFiles followSyms suffixes p = do
  isD <- doesDirectoryExist p
  isSymLink <-
#ifdef VERSION_unix
    isSymbolicLink `fmap` getSymbolicLinkStatus p
#else
    return False
#endif
  case isD of
    False -> return $ if matchingSuffix then [p] else []
    True ->
      if isSymLink && not followSyms
        then return []
        else do
          -- filter . .. and hidden files .*
          contents <- filter ((/=) '.' . head) `fmap` getDirectoryContents p
          concat `fmap` (mapM (dirToFiles followSyms suffixes . (</>) p) contents)
  where matchingSuffix = any (`isSuffixOf` p) suffixes
