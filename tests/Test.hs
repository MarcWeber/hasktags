module Main where

import Control.Monad
import Data.List
import System.Exit

import Hasktags
import Tags

import qualified Data.ByteString.Char8 as BS

import Test.HUnit

{- TODO
Test the library (recursive, caching, ..)
But that's less likely to break
-}

fileCases = [
    -- "99/expected_failures_testing_suite.hs",
    "1/testcase.hs",
    "2/testcase2.hs",
    "4/testcase4.hs",
    "8/test_case.hs",
    "10/twoblockcommentshs.hs",
    "12/twoblockcommentstogether.hs",
    "13/typesig.hs",
    "14/module.hs",
    "15/space.hs",
    "7/constructor.hs",
    "9/blockcomment.hs",
    "16/firstconstructor.hs",
    "17/substring.hs"
  ]

-- all comments should differ at the beginning
comments :: [BS.ByteString] -> String -> [String]
comments lines comment = filter (not . null) $ map hitOrEmpty lines
  where 
    c = BS.pack $ comment ++ " "
    hitOrEmpty :: BS.ByteString -> String
    hitOrEmpty bs =
      let ds = BS.dropWhile (== ' ') bs
      in if BS.isPrefixOf c ds
            then BS.unpack $ BS.drop (BS.length c) ds
            else ""

tagComments :: [BS.ByteString] -> String -> [String]
tagComments lns comment
  = map (takeWhile (not . (`elem` "\n\r "))) $ comments lns comment

testToBeFound foundTagNames toBeFound = 
        "these were not found" ~: [] ~=? filter (not . (`elem` foundTagNames)) toBeFound

testNotToBeFound foundTagNames notToBeFound = 
        "these should not have been found" ~: [] ~=? filter (`elem` foundTagNames) notToBeFound

testToBeFoundOnce foundTagNames list = 
        "these should have been found exactly one time" ~: [] ~=?  [ name | name <- list, 1 /= length (filter (==  name ) foundTagNames) ]

etagsToBeFound etags toBeFound =
        "these were not found on TAGS"
        ~: [] ~=? filter (not . (`isInfixOf` etags)) toBeFound

etagsNotToBeFound etags notToBeFound =
        "these should not have been found on TAGS"
        ~: [] ~=? filter (`isInfixOf` etags) notToBeFound

etagsOnceToBeFound etags list =
        "these should not have been found on TAGS"
        ~: [] ~=? [ name | name <- list, 1 /= length (infixes name etags)]

infixes :: Eq a => [a] -> [a] -> [[a]]
infixes needle haystack = filter (isPrefixOf needle) (tails haystack)

createTestCase filename = do
  bs <- BS.readFile ("testcases/" ++ filename)
  let lines = BS.lines bs
  let fd = findThingsInBS True filename bs
  let FileData _ things = fd

  let foundTagNames = [name | FoundThing _ name _ <- things]
  let etags = etagsDumpFileData fd

  let testList = TestList [
          testToBeFound foundTagNames (tagComments lines "-- to be found"),
          testNotToBeFound foundTagNames (tagComments lines "-- not to be found"),
          testToBeFoundOnce foundTagNames (tagComments lines "-- once to be found"),
          etagsToBeFound etags (comments lines "-- TAGS to be found"),
          etagsNotToBeFound etags (comments lines "-- TAGS not to be found"),
          etagsOnceToBeFound etags (comments lines "-- TAGS once to be found")
        ]

  return $ filename ~: testList

main
  = do
    tests <- mapM createTestCase fileCases
    counts_ <- runTestTT $ TestList tests
    when (errors counts_ + failures counts_ > 0) exitFailure
