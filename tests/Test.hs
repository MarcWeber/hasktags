module Main where

import Hasktags
import Tags

import Control.Monad
import Data.List
import System.Directory
import System.Exit

import qualified Data.ByteString.Char8 as BS

import Test.HUnit

{- TODO
Test the library (recursive, caching, ..)
But that's less likely to break
-}

-- all comments should differ at the beginning
comments :: [BS.ByteString] -> String -> [String]
comments lns comment = filter (not . null) $ map hitOrEmpty lns
  where
    c = BS.pack $ comment ++ " "
    hitOrEmpty :: BS.ByteString -> String
    hitOrEmpty bs =
      let ds = BS.dropWhile (== ' ') bs
      in if c `BS.isPrefixOf` ds
            then BS.unpack $ BS.drop (BS.length c) ds
            else ""

tagComments :: [BS.ByteString] -> String -> [String]
tagComments lns comment
  = map (takeWhile (not . (`elem` "\n\r "))) $ comments lns comment

testToBeFound :: [String] -> [String] -> Test
testToBeFound foundTagNames toBeFound =
        "these were not found"
        ~: [] ~?= filter (not . (`elem` foundTagNames)) toBeFound

testNotToBeFound :: [String] -> [String] -> Test
testNotToBeFound foundTagNames notToBeFound =
        "these should not have been found"
        ~: [] ~=? filter (`elem` foundTagNames) notToBeFound

testToBeFoundOnce :: [String] -> [String] -> Test
testToBeFoundOnce foundTagNames list =
        "these should have been found exactly one time"
        ~: []
          ~=? [name
            | name <- list, 1 /= length (filter (==  name ) foundTagNames)]

etagsToBeFound :: String -> [String] -> Test
etagsToBeFound etags toBeFound =
        "these were not found on TAGS"
        ~: [] ~=? filter (not . (`isInfixOf` etags)) toBeFound

etagsNotToBeFound :: String -> [String] -> Test
etagsNotToBeFound etags notToBeFound =
        "these should not have been found on TAGS"
        ~: [] ~=? filter (`isInfixOf` etags) notToBeFound

etagsToBeFoundOnce :: String -> [String] -> Test
etagsToBeFoundOnce etags list =
        "these should not have been found on TAGS"
        ~: [] ~=? [ name | name <- list, 1 /= length (infixes name etags)]

infixes :: Eq a => [a] -> [a] -> [[a]]
infixes needle haystack = filter (isPrefixOf needle) (tails haystack)

createTestCase :: FilePath -> IO Test
createTestCase filename = do
  bs <- BS.readFile filename
  let lns = BS.lines bs
  let fd = findThingsInBS True filename bs
  let FileData _ things = fd

  let foundTagNames = [name | FoundThing _ name _ _ _ <- things]
  let etags = etagsDumpFileData fd

  let testList = TestList [
          testToBeFound foundTagNames (tagComments lns "-- to be found"),
          testNotToBeFound foundTagNames (tagComments lns "-- not to be found"),
          testToBeFoundOnce
            foundTagNames
            (tagComments lns "-- once to be found"),
          etagsToBeFound etags (comments lns "-- TAGS to be found"),
          etagsNotToBeFound etags (comments lns "-- TAGS not to be found"),
          etagsToBeFoundOnce etags (comments lns "-- TAGS once to be found")
        ]

  return $ filename ~: testList

main :: IO ()
main
  = do
    setCurrentDirectory "testcases"
    files <- getDirectoryContents "."
    tests <- mapM createTestCase $ filter (not . (`elem` [".", "..", "expected_failures_testing_suite.hs"])) files
    counts_ <- runTestTT $ TestList tests
    when (errors counts_ + failures counts_ > 0) exitFailure
