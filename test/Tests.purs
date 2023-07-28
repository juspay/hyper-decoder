module Test.Main where

import Prelude
import Data.Array
import Test.QuickCheck (quickCheck)

main = quickCheck \n -> n +1 == 1 + n
