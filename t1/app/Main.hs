module Main where

import Prelude(Bool(..), Show(..), Eq(..), (print))
import System.IO

true :: Bool
true = True

false :: Bool
false = False

not :: Bool -> Bool
not True = False
not False = True

and :: Bool -> Bool -> Bool
and True  x = x
and False _ = False

or :: Bool -> Bool -> Bool
or False x = x
or True  _ = True

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  t _ = t
ifThenElse False _ e = e
asdasd
a
main :: IO ()
main = printf "hi"
