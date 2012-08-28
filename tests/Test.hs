module Main where

import Control.Monad
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
tagComments :: [BS.ByteString] -> String -> [String]
tagComments lines comment = filter (not . null) $ map hitOrEmpty lines
  where 
    c = BS.pack $ comment ++ " "
    hitOrEmpty :: BS.ByteString -> String
    hitOrEmpty bs =
      let ds = BS.dropWhile (== ' ') bs
      in if BS.isPrefixOf c ds
            then 
              let bs2 = BS.drop (BS.length c) ds
                  r = BS.takeWhile (/= ' ') bs2
              in if BS.all (`elem` "\n\r ") (BS.drop (BS.length r)  bs2)
                    then BS.unpack $ r
                    else error $  "bad - failed parsing tag line " ++ BS.unpack bs2
            else ""

testToBeFound foundTagNames toBeFound = 
        "these were not found" ~: [] ~=? filter (not . (`elem` foundTagNames)) toBeFound

testNotToBeFound foundTagNames notToBeFound = 
        "these should not have been found" ~: [] ~=? filter (`elem` foundTagNames) notToBeFound

testToBeFoundOnce foundTagNames list = 
        "these should have been found exactly one time" ~: [] ~=?  [ name | name <- list, 1 /= length (filter (==  name ) foundTagNames) ]

createTestCase filename = do
  bs <- BS.readFile ("testcases/" ++ filename)
  let lines = BS.lines bs
  let FileData _ things = findThingsInBS True filename bs

  let foundTagNames = [name | FoundThing _ name _ <- things]

  let testList = TestList [
          testToBeFound foundTagNames (tagComments lines "-- to be found"),
          testNotToBeFound foundTagNames (tagComments lines "-- not to be found"),
          testToBeFoundOnce foundTagNames (tagComments lines "-- once to be found")
        ]

  return $ filename ~: testList

main
  = do
    tests <- mapM createTestCase fileCases
    counts_ <- runTestTT $ TestList tests
    when (errors counts_ + failures counts_ > 0) exitFailure
