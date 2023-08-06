{-|
  This is a program to traverse a record and get the keys and types of the fields.
  Supports all primitive types like String, Int, Char, Number, Boolean along with nested Records and Arrays.
-}
module Main where

import Prelude

import Data.Show (class Show)
import Foreign (Foreign)
import DecodedVal
import Effect.Console (log)
import Data.Unit (unit)
import Data.Generic.Rep
import Foreign.Generic.Class (class Decode, class Encode, decode, encode)
import Types (BigType(..), Cast(..))
import HyperDecode (decodeForeign, class HyperDecode)
import Foreign.Object
import Tests
import Data.Maybe
import Data.Tuple
import Data.Array (length, partition)

foreign import movieData :: Unit -> Foreign
foreign import startProfile :: Unit -> Unit
foreign import endProfile :: Unit -> Unit

--getMovieData :: Either (Array Movie)
getMovieData _ = let
    _ = startProfile unit
    val = decodeForeign (movieData unit)
    _ = endProfile unit
    in val

val :: String
val =
    case getMovieData unit of
        DecodeErr x -> x
        Val (x :: Array BigType) -> show x

foreign import getTimeLine :: Unit -> Foreign

timeLine :: String
timeLine = case decodeForeign (getTimeLine unit) :: DecodedVal (Object String) of
    DecodeErr err -> err
    Val       val -> show val

input1 :: BigType
input1 = BigType
  { id: Just 1
  , title: "Movie 1"
  , rating: 4.5
  , year: 2022
  , cast: [Actor "Actor 1", Actor "Actor 2", Actor "Actor 3"]
  , reviews: { count: 10, reviewers: Just [{name : "Reviewer 1", id : Nothing}, {name : "Reviewer 2", id : Nothing}, {name : "Reviewer 3", id : Nothing}] }
  }

input2 :: BigType
input2 = BigType
  { id: Nothing
  , title: "Movie 2"
  , rating: 3.7
  , year: 2019
  , cast: [Actor "Actor 4", Actor "Actor 5"]
  , reviews: { count: 5, reviewers: Nothing }
  }

psVal = [input1, input1, input2]

checkDecode :: Unit -> Tuple Boolean String
checkDecode _ = case getMovieData unit :: DecodedVal (Array BigType) of
    DecodeErr err -> Tuple false err
    Val val -> Tuple (psVal == val) ""

--main = log $ show (checkDecode unit)

main =
    let {yes, no} = partition identity decodeTests
    in log $ "Passed " <> (show $ length yes) <> ", Failed " <> (show $ length no)

foreign import perfectInt :: Foreign
foreign import intWithDecimal :: Foreign
foreign import float :: Foreign

test :: forall a b. HyperDecode b => Encode a => Eq b => a -> DecodedVal b -> Boolean
test x y = case (decodeForeign $ encode x), y of
             DecodeErr _, DecodeErr _ -> true
             Val x, Val y -> x == y
             _, _ -> false

infixr 1 test as ===

decodeTests =
    [ perfectInt === Val 123
    , intWithDecimal === Val 123
    , float === (DecodeErr "" :: DecodedVal Int)
    , perfectInt === Val 123.0
    , intWithDecimal === Val 123.0
    , float === Val 123.45
    , "" === (DecodeErr "" :: DecodedVal Int)
    ]
