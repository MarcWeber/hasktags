module TestCommon where

-- base
import System.Exit

-- process
import System.Cmd

-- HUnit
import Test.HUnit

testMain :: String -> (String -> IO ()) -> IO ()
testMain name f
  = do
    code
      <- rawSystem
        "dist/build/hasktags/hasktags"
        ["--ignore-close-implementation", "-e", "tests/data/" ++ name]
    if code /= ExitSuccess
      then exitFailure
      else readFile "TAGS" >>= f

empty :: String -> IO ()
empty = (@=? 2) . length . lines
