module Tests where

import Prelude
import Data.Array


runTests :: Array (Unit -> Boolean) -> Boolean
runTests = not <<< all (\x -> x unit)

foreign import cmpr :: forall a. a -> a -> Boolean 
