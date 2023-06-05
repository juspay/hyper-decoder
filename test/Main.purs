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
import Foreign.Generic.Class (decode, class Decode)
import Types (BigType(..), Cast(..))
import HyperDecode
import Foreign.Object
import Tests
import Data.Maybe
import Data.Tuple
 
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
  , reviews: { count: 10, reviewers: Just ["Reviewer 1", "Reviewer 2", "Reviewer 3"] }
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

main = log $ show (checkDecode unit)