{-# LANGUAGE DeriveDataTypeable #-}
-- everyting tagfile related ..
-- this should be moved into its own library (after cleaning up most of it ..)
-- yes, this is still specific to hasktags :(
module Tags where
import Data.Char
import Data.List
import Data.Data

import System.IO
import Control.Monad

-- my words is mainly copied from Data.List.
-- difference abc::def is recognized as three words
-- `abc` is recognized as "`" "abc" "`"
mywords :: Bool -> String -> [String]
mywords spaced s =  case rest of
                        ')':xs -> (blanks' ++ ")") : mywords spaced xs
			"" -> []
                        '{':'-':xs -> (blanks' ++ "{-") : mywords spaced xs
                        '-':'}':xs -> (blanks' ++ "-}") : mywords spaced xs
                        '{':xs -> (blanks' ++ "{") : mywords spaced xs
                        '(':xs -> (blanks' ++ "(") : mywords spaced xs
                        '`':xs -> (blanks' ++ "`") : mywords spaced xs
                        '=':'>':xs -> (blanks' ++ "=>") : mywords spaced xs
                        '=':xs -> (blanks' ++ "=") : mywords spaced xs
                        ',':xs -> (blanks' ++ ",") : mywords spaced xs
                        ':':':':xs -> (blanks' ++ "::") : mywords spaced xs
			s' -> (blanks' ++ w) : mywords spaced s''
			      where (w, s'') = myBreak s'
				    myBreak [] = ([],[])
				    myBreak (':':':':xs) = ([], "::"++xs)
				    myBreak (')':xs) = ([],')':xs)
                                    myBreak ('(':xs) = ([],'(':xs)
				    myBreak ('`':xs) = ([],'`':xs)
				    myBreak ('=':xs) = ([],'=':xs)
				    myBreak (',':xs) = ([],',':xs)
                                    myBreak xss@(x:xs)
                                      | isSpace x
                                        = if spaced
                                          then ([], xss)
                                          else ([], dropWhile isSpace xss)
                                      | otherwise = let (a,b) = myBreak xs
						    in  (x:a,b)
                    where blanks' = if spaced then blanks else ""
                          (blanks, rest) = span {-partain:Char.-}isSpace s


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
-- I'm not sure wether I've used the right names.. but I hope you fix it / get
-- what I mean
data FoundThingType
  = FTFuncTypeDef
    | FTFuncImpl
    | FTType
    | FTData
    | FTDataGADT
    | FTNewtype
    | FTClass
    | FTModule
    | FTCons
    | FTOther
    | FTConsAccessor
    | FTConsGADT
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

getfoundthings :: FileData -> [FoundThing]
getfoundthings (FileData _ things) = things

ctagEncode :: Char -> String
ctagEncode '/' = "\\/"
ctagEncode '\\' = "\\\\"
ctagEncode a = [a]

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


-- stuff for dealing with ctags output format
writectagsfile :: Handle -> Bool -> [FileData] -> IO ()
writectagsfile ctagsfile extended filedata = do
    let things = concatMap getfoundthings filedata
    when extended
         (do hPutStrLn
                 ctagsfile
               $ "!_TAG_FILE_FORMAT\t2\t/extended format; --format=1 will not "
                 ++ "append ;\" to lines/"
             hPutStrLn
               ctagsfile
               "!_TAG_FILE_SORTED\t1\t/0=unsorted, 1=sorted, 2=foldcase/"
             hPutStrLn ctagsfile "!_TAG_PROGRAM_NAME\thasktags")
    mapM_ (hPutStrLn ctagsfile . dumpthing extended) (sortThings things)

sortThings :: [FoundThing] -> [FoundThing]
sortThings = sortBy comp
  where
        comp (FoundThing _ a (Pos f1 l1 _ _)) (FoundThing _ b (Pos f2 l2 _ _)) =
            c (c (compare a b) (compare f1 f2)) (compare l1 l2)
        c a b = if a == EQ then b else a


-- stuff for dealing with etags output format

writeetagsfile :: Handle -> [FileData] -> IO ()
writeetagsfile etagsfile = mapM_ (hPutStr etagsfile . etagsDumpFileData)

etagsDumpFileData :: FileData -> String
etagsDumpFileData (FileData filename things) =
    "\x0c\n" ++ filename ++ "," ++ show thingslength ++ "\n" ++ thingsdump
    where thingsdump = concatMap etagsDumpThing things
          thingslength = length thingsdump

etagsDumpThing :: FoundThing -> String
etagsDumpThing (FoundThing _ name (Pos _filename line token fullline)) =
  let wrds = mywords True fullline
  in concat (take token wrds ++ map (take 1) (take 1 $ drop token wrds))
        ++ "\x7f"
        ++ name ++ "\x01"
        ++ show line ++ "," ++ show (line + 1) ++ "\n"

