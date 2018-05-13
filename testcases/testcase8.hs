-- to be found Main
-- to be found ABC
-- to be found ABCD
-- to be found (@=?)
-- to be found (@=:)
-- to be found dummy
-- to be found main
module Main where

data ABC a = ABC a
class (Show a) => (ABCD a) where
  abcshow :: (ABC a)

(@=?), (@=:) :: (Eq a, Show a) => a -> a -> Int

expected @=? actual = undefined
expected @=: actual = undefined

dummy = "a"

main = print "abc"

