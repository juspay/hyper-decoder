{-|
  This is a program to traverse a record and get the keys and types of the fields.
  Supports all primitive types like String, Int, Char, Number, Boolean along with nested Records and Arrays.
-}
module Main where

import Prelude

import Data.Show (class Show)
import Foreign (Foreign, ForeignError)
import DecodedVal
import Effect.Console (log)
import Data.Unit (unit)
import Data.Generic.Rep
import Foreign.Generic.Class (class Decode, class Encode, decode, encode)
import Types (BigType(..), Cast(..))
import HyperDecode (class HyperDecode, decodeForeign, hyperDecode, partialDecode)
import Foreign.Object
import Foreign.Object as Obj
import Data.Maybe
import Data.Tuple
import Data.Array (all, length, partition)
import Effect (Effect)
import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.List.Types (NonEmptyList)

foreign import movieData :: Unit -> Foreign
foreign import startProfile :: Unit -> Unit
foreign import endProfile :: Unit -> Unit
foreign import getTimeLine :: Unit -> Foreign
foreign import partialData :: Foreign
foreign import perfectInt :: Foreign
foreign import intWithDecimal :: Foreign
foreign import float :: Foreign

main :: Effect Unit
main =
    let {yes, no} = partition (\x -> x) tests
    in log $ "passed : " <> (show $ length yes) <> " , failed : " <> (show $ length no)

test :: forall a. Eq a => Tuple (DecodedVal a) (DecodedVal a) -> Boolean
test (Tuple x y) = case x, y of
   DecodeErr _, DecodeErr _ -> true
   Val x , Val y -> x == y
   _ ,  _ -> false

completeVal = BigType {id : Just 3, title : "Banshee", rating : 4.5, year : 2016, cast : [], reviews : {count : 0, reviewers : Nothing}}

getVal (BigType x) = x

infixr 0 Tuple as ===

partialForeignDecode :: forall a b. (HyperDecode a) => Encode b => a -> b -> DecodedVal a
partialForeignDecode a b = partialDecode a (encode b) Val DecodeErr

infixr 1 partialForeignDecode as ++

d :: forall a b. (HyperDecode a) => Encode b => b -> DecodedVal a
d = encode >>> decodeForeign

tests :: Array Boolean
tests =
    [ -- partial decode
      test $ Just {id : 99, name : "sd"} ++ {name : "gf"} === Val $ Just {id : 99, name : "gf"}
    , test $ Just {id : 99, name : "sd"} ++ {name : "sd", id : 99} === Val $ Just {id : 99, name : "sd"}
    , test $ Nothing ++ {name : "sd", id : 99} === Val $ Just {id : 99, name : "sd"}
    , test $ (Nothing :: Maybe {}) ++ 3 === Val $ Just {}
    , test $ (Just {}) ++ 3 === Val $ Just {}
    , test $ {} ++ {name : "af"} === Val {}
    , test $ {name : "af"} ++ {age : 99} === Val {name : "af"}
    , test $ 10 ++ 11 === Val 11
    , test $ (10 :: Int) ++ 10.2 === DecodeErr ""
    , test $ "" ++ 12 === DecodeErr ""
    , test $ {name : "af"} ++ 123 === DecodeErr ""
    , test $ {name : "af"} ++ (Nothing :: Maybe {}) === DecodeErr ""
    , test $ completeVal ++ {id : Just 10} === Val $ BigType ((getVal completeVal)  {id = Just 10})
    , test $ completeVal ++ (Nothing :: Maybe {}) === DecodeErr ""
    -- hyper decode int
    , test $ d perfectInt === Val 123
    , test $ d intWithDecimal === Val 123
    , test $ d float === (DecodeErr "" :: DecodedVal Int)
    , test $ d perfectInt === Val 123.0
    , test $ d intWithDecimal === Val 123.0
    , test $ d float === Val 123.45
    , test $ d "" === (DecodeErr "" :: DecodedVal Int)

    -- hyper decode array
    , test $ d [1, 2, 3] === Val [1, 2, 3]
    -- hyper decode array foreign
    , case d [encode 1, encode 2, encode 3] :: DecodedVal (Array Foreign) of
         DecodeErr _ -> false
         Val arr -> test $ d arr === Val [1, 2, 3]

    -- hyper decode object
    , test $ d {lat : 100.23, long: 392.123} === Val (Obj.insert "long" 392.123 $ Obj.singleton "lat" 100.23)
    -- hyper decode object foreign
    , case d {lat : 100.23, long: 392.123} :: DecodedVal (Object Foreign) of
         DecodeErr _ -> false
         Val obj -> test $ d obj === Val (Obj.insert "long" 392.123 $ Obj.singleton "lat" 100.23)
    ]
