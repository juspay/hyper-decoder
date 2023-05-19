{-|
  This is a program to traverse a record and get the keys and types of the fields.
  Supports all primitive types like String, Int, Char, Number, Boolean along with nested Records and Arrays.
-}
module Main where

import Prelude

import Data.Show (class Show)
import Data.Array (tail, (:))
import Foreign (Foreign)
import LiteDecode.Decode
import Main.DecodeError
import Effect.Console (log)
import Data.Newtype (wrap)
import Data.Maybe (Maybe)
import Data.Unit (unit)
import Data.Generic.Rep
import Foreign.Generic.Class (decode, class Decode)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Types (BigType)
import Type.Proxy (Proxy)
import Chain (decodeForeign, decodeString)
 
foreign import movieData :: Unit -> Foreign
foreign import startProfile :: Unit -> Unit
foreign import endProfile :: Unit -> Unit

--getMovieData :: Either (Array Movie)
getMovieData = let
    _ = startProfile unit
--    val = decodeString "[{\"id\": 10, \"cast\":[\"asdf\",\"123\"],\"title\":\"afg\",\"year\":2020,\"rating\":7.3,\"reviews\":{\"count\":10}}]"
    val = decodeForeign (movieData unit)
    _ = endProfile unit
    in val

--
--val :: String
--val =
--    case runExcept getMovieData of
--        Left _ -> ""
--        Right (x :: Array BigType) -> show x

val :: String
val =
    case getMovieData of
        DecodeErr x -> x
        Val (x :: Array BigType) -> show x

main = log $ "" --val