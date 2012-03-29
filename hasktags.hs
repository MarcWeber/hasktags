{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where
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
--import Debug.Trace


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
  My first fix was checking for \\begin occurence (doesn't work because HUnit is using > but no \\begin)
  Further ideas: 
    * use unlit executable distributed with ghc or the like and check for errors?
      (Will this work if cpp is used as well ?)
    * Remove comments before checking for '> ..'
      does'nt work because {- -} may be unbalanced in literate comments
  So my solution is : take file extension and keep guessing code for all unkown files
-}
 

-- Reference: http://ctags.sourceforge.net/FORMAT
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


-- | getMode takes a list of modes and extract the mode with the
--   highest precedence.  These are as follows: Both, CTags, ETags
--   The default case is Both.
getMode :: [Mode] -> Mode
getMode [] = BothTags
getMode xs = maximum xs

-- | getOutFile scan the modes searching for output redirection
--   if not found, open the file with name passed as parameter.
--   Handle special file -, which is stdout
getOutFile :: String -> IOMode -> [Mode] -> IO Handle
getOutFile _           _        ((OutRedir "-"):_) = return stdout
getOutFile _           openMode ((OutRedir f):_)   = openFile f openMode
getOutFile name        openMode (_:xs)             = getOutFile name openMode xs
getOutFile defaultName openMode []                 = openFile defaultName openMode

data Mode = ExtendedCtag
		  | IgnoreCloseImpl
          | ETags 
          | CTags 
          | BothTags 
          | Append 
          | OutRedir String
          | CacheFiles
          | Help
          deriving (Ord, Eq, Show)

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

type FileName = String

type ThingName = String

-- The position of a token or definition
data Pos = Pos
                FileName -- file name
                Int      -- line number
                Int      -- token number
                String   -- string that makes up that line
   deriving (Show,Eq,Typeable,Data)

-- A definition we have found
-- I'm not sure wether I've used the right names.. but I hope you fix it / get what I mean
data FoundThingType = FTFuncTypeDef | FTFuncImpl | FTType | FTData | FTDataGADT | FTNewtype | FTClass | FTModule | FTCons | FTOther | FTConsAccessor | FTConsGADT
  deriving (Eq,Typeable,Data)

instance Show FoundThingType where
  show FTFuncTypeDef = "ft"
  show FTFuncImpl = "fi"
  show FTType = "t"
  show FTData = "d"
  show FTDataGADT = "d_gadt"
  show FTNewtype = "nt"
  show FTClass = "c"
  show FTModule = "m"
  show FTCons = "cons"
  show FTConsGADT = "c_gadt"
  show FTConsAccessor = "c_a"
  show FTOther = "o"

data FoundThing = FoundThing FoundThingType ThingName Pos
        deriving (Show,Eq,Typeable,Data)

-- Data we have obtained from a file
data FileData = FileData FileName [FoundThing]
  deriving (Typeable,Data,Show)

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


-- stuff for dealing with ctags output format

writectagsfile :: Handle -> Bool -> [FileData] -> IO ()
writectagsfile ctagsfile extended filedata = do
    let things = concatMap getfoundthings filedata
    when extended
         (do hPutStrLn ctagsfile "!_TAG_FILE_FORMAT\t2\t/extended format; --format=1 will not append ;\" to lines/"
             hPutStrLn ctagsfile "!_TAG_FILE_SORTED\t1\t/0=unsorted, 1=sorted, 2=foldcase/"
             hPutStrLn ctagsfile "!_TAG_PROGRAM_NAME\thasktags")
    mapM_ (hPutStrLn ctagsfile . dumpthing extended) (sortThings things)

sortThings :: [FoundThing] -> [FoundThing]
sortThings = sortBy comp
  where 
        comp (FoundThing _ a (Pos f1 l1 _ _)) (FoundThing _ b (Pos f2 l2 _ _)) =
            c (c (compare a b) $ (compare f1 f2)) (compare l1 l2)
        c a b = if a == EQ then b else a


getfoundthings :: FileData -> [FoundThing]
getfoundthings (FileData _ things) = things

-- | Dump found tag in normal or extended (read : vim like) ctag
-- line
dumpthing :: Bool -> FoundThing -> String
dumpthing False (FoundThing _ name (Pos filename line _ _)) =
    name ++ "\t" ++ filename ++ "\t" ++ show (line + 1)
dumpthing True (FoundThing kind name (Pos filename line _ lineText)) =
    name ++ "\t" ++ filename
         ++ "\t/^" ++ concatMap ctagEncode lineText
         ++ "$/;\"\t" ++ show kind
         ++ "\tline:" ++ show (line + 1)

ctagEncode :: Char -> String
ctagEncode '/' = "\\/"
ctagEncode '\\' = "\\\\"
ctagEncode a = [a]

-- stuff for dealing with etags output format

writeetagsfile :: Handle -> [FileData] -> IO ()
writeetagsfile etagsfile = mapM_ (hPutStr etagsfile . etagsDumpFileData)

etagsDumpFileData :: FileData -> String
etagsDumpFileData (FileData filename things) =
    "\x0c\n" ++ filename ++ "," ++ show thingslength ++ "\n" ++ thingsdump
    where thingsdump = concatMap etagsDumpThing things
          thingslength = length thingsdump

etagsDumpThing :: FoundThing -> String
etagsDumpThing (FoundThing _ _name (Pos _filename line token fullline)) =
        concat (take (token + 1) $ spacedwords fullline)
        ++ "\x7f" ++ show line ++ "," ++ show (line + 1) ++ "\n"


-- like "words", but keeping the whitespace, and so letting us build
-- accurate prefixes

spacedwords :: String -> [String]
spacedwords [] = []
spacedwords xs = (blanks ++ wordchars) : spacedwords rest2
    where (blanks,rest) = span isSpace xs
          (wordchars,rest2) = break isSpace rest

-- Find the definitions in a file, or load from cache if the file
-- hasn't changed since last time.
findWithCache :: Bool -> Bool -> FileName -> IO FileData
findWithCache cache ignoreCloseImpl filename = do
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
          filedata <- findthings ignoreCloseImpl filename
          when cache (writeFile cacheFilename (encodeJSON filedata))
          return filedata

-- Find the definitions in a file
findthings :: Bool -> FileName -> IO FileData
findthings ignoreCloseImpl filename = do
        aslines <- fmap ( lines . evaluate . BS.unpack) $ BS.readFile filename

        let stripNonHaskellLines = let
                  emptyLine = all (all isSpace . tokenString)
                            . filter (not . isNewLine Nothing)
                  cppLine (_nl:t:_) = ("#" `isPrefixOf`) $ tokenString t
                  cppLine _ = False
                in filter (not . emptyLine) . filter (not . cppLine)

        --  remove -- comments, then break each line into tokens (adding line numbers)
        --  then remove {- -} comments
        --  split by lines again ( to get indent
        let (fileLines, numbers) = unzip . fromLiterate filename $ zip aslines [0..]
        let tokenLines =
                      stripNonHaskellLines
                      $ stripslcomments
                      $ splitByNL Nothing
                      $ stripblockcomments
                      $ concat
                      $ zipWith3 (withline filename)
                                 (map ( filter (not . all isSpace) . mywords) fileLines)
                                 fileLines
                                 numbers


        -- TODO  ($defines / empty lines etc)
        -- separate by top level declarations (everything starting with the
        -- same topmost indentation is what I call section here)
        -- so that z in
        -- let x = 7
        --     z = 20
        -- won't be found as function 
        let sections = map tail -- strip leading NL (no longer needed 
                       $ filter (not . null)
                       $ splitByNL (Just (getTopLevelIndent tokenLines) )
                       $ concat tokenLines
        -- only take one of
        -- a 'x' = 7
        -- a _ = 0
        let filterAdjacentFuncImpl = nubBy (\(FoundThing t1 n1 (Pos f1 _ _ _)) 
                                             (FoundThing t2 n2 (Pos f2 _ _ _))
                                             -> f1 == f2 && n1 == n2 && t1 == FTFuncImpl && t2 == FTFuncImpl )

        let iCI = if ignoreCloseImpl 
              then nubBy (\(FoundThing _ n1 (Pos f1 l1 _ _)) 
                         (FoundThing _ n2 (Pos f2 l2 _ _))
                         -> f1 == f2 && n1 == n2  && ( ( <= 7 ) $ abs $ l2 - l1))
              else id
        return $ FileData filename $ iCI $ filterAdjacentFuncImpl $ concatMap findstuff sections

  where 
        evaluate :: String -> String        
        evaluate [] = []
        evaluate (c:cs) = c `seq` c:evaluate cs
	-- my words is mainly copied from Data.List.
	-- difference abc::def is recognized as three words
        -- `abc` is recognized as "`" "abc" "`"
	mywords :: String -> [String]
	mywords ('{':xs) = "{" : mywords xs
	mywords ('(':xs) = "(" : mywords xs
	mywords ('`':xs) = "`" : mywords xs
	mywords ('=':'>':xs) = "=>" : mywords xs
	mywords ('=':xs) = "=" : mywords xs
	mywords (',':xs) = "," : mywords xs
	mywords (':':':':xs) = "::" : mywords xs
	mywords s	=  case dropWhile {-partain:Char.-}isSpace s of
                                ')':xs -> ")" : mywords xs
				"" -> []
				s' -> w : mywords s''
				      where (w, s'') = myBreak s'
					    myBreak [] = ([],[])
					    myBreak (':':':':xs) = ([], "::"++xs)
					    myBreak (')':xs) = ([],')':xs)
                                            myBreak ('(':xs) = ([],'(':xs)
					    myBreak ('`':xs) = ([],'`':xs)
					    myBreak ('=':xs) = ([],'=':xs)
					    myBreak (',':xs) = ([],',':xs)
					    myBreak (' ':xs) = ([],xs);
					    myBreak (x:xs) = let (a,b) = myBreak xs 
							     in  (x:a,b)
	
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
stripslcomments = let f ((NewLine _):(Token "--" _):_) = False
                      f _ = True
                  in filter f

stripblockcomments :: [Token] -> [Token]
stripblockcomments ((Token "\\end{code}" _):xs) = afterlitend xs
stripblockcomments ((Token "{-" _):xs) = afterblockcomend xs
stripblockcomments (x:xs) = x:stripblockcomments xs
stripblockcomments [] = []

afterlitend :: [Token] -> [Token]
afterlitend (Token "\\begin{code}" _ : xs) = xs
afterlitend (_ : xs) = afterlitend xs
afterlitend [] = []

afterblockcomend :: [Token] -> [Token]
afterblockcomend (t:xs)
 | contains "-}" (tokenString t) = xs
 | otherwise           = afterblockcomend xs
afterblockcomend [] = []


-- does one string contain another string

contains :: Eq a => [a] -> [a] -> Bool
contains sub = any (isPrefixOf sub) . tails

-- actually pick up definitions

findstuff :: [Token] -> [FoundThing]
findstuff ((Token "module" _):(Token name pos):_) =
        [FoundThing FTModule name pos] -- nothing will follow this section
findstuff ((Token "data" _):(Token name pos):xs)
        | any ( (== "where"). tokenString ) xs -- GADT 
            -- TODO will be found as FTCons (not FTConsGADT), the same for functions - but they are found :) 
            = FoundThing FTDataGADT name pos : getcons2 xs ++ fromWhereOn xs -- ++ (findstuff xs)
        | otherwise = FoundThing FTData name pos : getcons FTData (trimNewlines xs)-- ++ (findstuff xs)
findstuff ((Token "newtype" _):ts@(((Token name pos)):_)) =
        FoundThing FTNewtype name pos : getcons FTCons (trimNewlines ts)-- ++ (findstuff xs)
        -- FoundThing FTNewtype name pos : findstuff xs
findstuff ((Token "type" _):(Token name pos):xs) =
        FoundThing FTType name pos : findstuff xs
findstuff ((Token "class" _):xs) = case break ((== "where").tokenString) xs of
        (ys,[]) -> maybeToList $ className ys
        (_,r) -> maybe [] (:fromWhereOn r) $ className xs
    where isParenOpen (Token "(" _) = True
          isParenOpen _ = False
          className lst = case (head . dropWhile isParenOpen . reverse . takeWhile ((/= "=>").tokenString) . reverse) lst of
            (Token name p) -> Just $ FoundThing FTClass name p
            _ -> Nothing
findstuff xs = findFunc xs ++ findFuncTypeDefs [] xs

findFuncTypeDefs :: [Token] -> [Token] -> [FoundThing]
findFuncTypeDefs found (t@(Token _ _): Token "," _ :xs) =
          findFuncTypeDefs (t : found) xs
findFuncTypeDefs found (t@(Token _ _): Token "::" _ :_) =
          map (\(Token name p) -> FoundThing FTFuncTypeDef name p) (t:found)
findFuncTypeDefs found (Token "(" _ :xs) =
          case break myBreakF xs of
            (inner@((Token _ p):_), _:xs') ->
              let merged = Token ( concatMap (\(Token x _) -> x) inner ) p
              in findFuncTypeDefs found $ merged : xs'
            _ -> []
    where myBreakF (Token ")" _) = True
          myBreakF _ = False          
findFuncTypeDefs _ _ = []

fromWhereOn :: [Token] -> [FoundThing]
fromWhereOn [] = []
fromWhereOn [_] = []
fromWhereOn (_: xs@((NewLine _):_)) =
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
findInfix x = case dropWhile ((/= "`"). tokenString) (takeWhile ( (/= "=") . tokenString) x) of
          _:(Token name p):_ -> [FoundThing FTFuncImpl name p]
          _ -> []


findF :: [Token] -> [FoundThing]
findF ((Token name p):xs) =
    [FoundThing FTFuncImpl name p | any (("=" ==) . tokenString) xs]
findF _ = []

tail' :: [a] -> [a]
tail' (_:xs) = xs
tail' [] = []

-- get the constructor definitions, knowing that a datatype has just started

getcons :: FoundThingType -> [Token] -> [FoundThing]
getcons ftt ((Token "=" _):(Token name pos):xs) =
        FoundThing ftt name pos : getcons2 xs
getcons ftt (_:xs) = getcons ftt xs
getcons _ [] = []


getcons2 :: [Token] -> [FoundThing]
getcons2 ((Token name pos):(Token "::" _):xs) =
        FoundThing FTConsAccessor name pos : getcons2 xs
getcons2 ((Token "=" _):_) = []
getcons2 ((Token "|" _):(Token name pos):xs) =
        FoundThing FTCons name pos : getcons2 xs
getcons2 (_:xs) = getcons2 xs
getcons2 [] = []


splitByNL :: (Maybe Int) -> [Token] -> [[Token]]
splitByNL maybeIndent (nl@(NewLine _):ts) =
  let (a,b) = break (isNewLine maybeIndent) ts
  in (nl : a) : splitByNL maybeIndent b
splitByNL _ _ = []

getTopLevelIndent :: [[Token]] -> Int
getTopLevelIndent [] = 0 -- (no import found , assuming indent 0 : this can be
                         -- done better but should suffice for most needs
getTopLevelIndent (x:xs) = if any ((=="import") . tokenString) x
                          then let ((NewLine i):_) = x in i
                          else getTopLevelIndent xs

-- removes literate stuff if any line '> ... ' is found and any word is \begin (hglogger has ^> in it's commetns)
fromLiterate :: FilePath -> [(String, Int)] -> [(String, Int)]
fromLiterate file lines = 
  let literate = [ (ls, n) |  ('>':ls, n) <- lines ]
  in if ".lhs" `isSuffixOf` file && (not . null $ literate) then literate -- not . null literate because of Repair.lhs of darcs 
      else if (".hs" `isSuffixOf` file)
            || (null literate || not ( any ( any ("\\begin" `isPrefixOf`). words . fst) lines))
        then lines
        else literate

{- testcase:

checkToBeFound(){
  toBeFound=$(sed -n 's/-- to be found\s*//p' testcase.hs)
  for i in $toBeFound; do
    grep -l $i tags 2>&1 > /dev/null || echo "tag $i was not found"
  done
  echo -n "to be found ocunt: "
  echo "$toBeFound" | wc -l
}

-- to be found A.B.testcase
module A.B.testcase(module System.FilePath.Windows) where
    import asdf

-- to be found Request
-- to be found Request2
-- to be found rqBody
-- to be found rqMethod
-- to be found rqPeer
-- to be found Request3
    data Request = Request2 { rqMethod::Method,
                             rqBody    :: RqBody,
                             rqPeer    :: Host
                           }
                  | Request3
deriving(Show,Read,Typeable)
    --  http://hackage.haskell.org/trac/ghc/ticket/1184
    -- ! Convert Bool into another monad
-- to be found boolM
    boolM False = mzero

-- to be found sadlkfj
    sadlkfj
     = 7

-- to be found onlyTheFirstOne
    onlyTheFirstOne (x:xs) = 8
    onlyTheFirstOne [] = 8
-- to be found AC
    AC a b c d e f g = 7
-- to be found abc
    abc = let a = 7
              b = 8
              in a + b
            where x = 34
                  o = 423
-- to be found BB
-- to be found AA
    AA, BB :: Int


-- to be found foo
    ad `foo` oh = 90

-- to be found X
-- to be found xyz
    class (A a) => X a where
      xyz :: dummy
-- to be found Z
-- to be found o
    class (A a) => Z a where o :: Int

-- to be found ABC
    newtype ABC = Int
-- to be found DBM
    newtype IE.ISession sess => DBM mark sess a = DBM (ReaderT sess IO a)
-- to be found SAA
newtype Symbol = SAA String

-- TODO 

-- to be found =~
(=~)   :: (Regex rho) => String -> rho -> Bool
-}
