{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module HyperDecode where

import Prelude
import Foreign (Foreign, unsafeFromForeign)
import Type.RowList (class RowToList, RowList, Nil, Cons)
import Type.Proxy (Proxy(Proxy))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons, class Lacks)
import Data.Maybe (Maybe, Maybe(Just), Maybe(Nothing))
import DecodedVal (DecodedVal, DecodedVal(Val), DecodedVal(DecodeErr))
import Foreign.Object (Object)
import Foreign.Object (mapWithKey) as Object
import Foreign.Generic.Internal (isObject)
import Data.Newtype (class Newtype, wrap)
import Unsafe.Coerce

foreign import lookupVal :: Foreign -> String -> Foreign

foreign import tryCatch :: forall a b. Foreign -> (Foreign -> a) -> (a -> b) -> (String -> b) -> b

-- | The `HyperDecode` type class represents those types that can be created (decoded) from foreign values (any JS object).
-- | A decode function of HyperDecode takes three parameters.
-- |  1. Foreign object to decode
-- |  2. Function to call by passing a decoded value upon successful decode 
-- |  3. Function to call by passing a `String`, which expresses why decode is failed 
-- |
-- | While there are other good libraries like foreign-generic or purescript-argonaut which can get the job done, major problem `HyperDecode` addressing is decode speed.
-- |
-- | `HyperDecode` took the liberty of stripping off configurability of decode on record types.
-- | So to successfully decode a record, foreign object keys must be same as record keys.
class HyperDecode a where
    hyperDecode :: forall b. Foreign -> (a -> b) -> (String -> b) -> b

foreign import getType :: forall a. a -> String

primitiveTypeDecode :: forall a b. String -> Foreign -> (a -> b) -> (String -> b) -> b
primitiveTypeDecode expectedType obj success failure =
    if jsType == expectedType
        then success (unsafeCoerce obj)
        else failure ("type mismatch : expected " <> expectedType <> ", found " <> jsType)
    where
        jsType = getType obj

instance stringDecode :: HyperDecode String where
    hyperDecode = primitiveTypeDecode "string"

instance intDecode :: HyperDecode Int where
    hyperDecode = primitiveTypeDecode "number"

instance numberDecode :: HyperDecode Number where
    hyperDecode = primitiveTypeDecode "number"

instance bitDecode :: HyperDecode Boolean where
    hyperDecode = primitiveTypeDecode "boolean"

instance foreignDecode :: HyperDecode Foreign where
    hyperDecode obj success _ = success obj

instance objectDecode :: (HyperDecode a) => HyperDecode (Object a) where
    hyperDecode obj success failure = if isObject obj
        then tryCatch obj (\frn -> Object.mapWithKey (\_ -> (\x -> hyperDecode x identity shortCircuit)) (unsafeFromForeign frn)) success failure
        else failure "value is not an object"

foreign import arrDecodeImpl :: forall a b c. Foreign -> (Foreign -> a) -> (Array a -> c) -> (String -> c) -> c

foreign import shortCircuit :: forall a. String -> a

identity :: forall a.a -> a
identity x = x

foreign import isArray :: forall a. a -> Boolean

instance arrDecode :: (HyperDecode a) => HyperDecode (Array a) where
    hyperDecode obj success failure = 
        if isArray obj
            -- | array will be decoded by traversing the foreign object
            -- | any decode fail in the object will call shortCircuit, which will throw error
            -- | this error will be caught and failure function will be called
            then arrDecodeImpl obj (\x -> hyperDecode x (\y -> y) shortCircuit) success failure
            else failure "value is not an array"

foreign import isNullOrUndefined :: forall a. a -> Boolean

instance maybeDecode :: (HyperDecode a) => HyperDecode (Maybe a) where
    hyperDecode obj success failure =
        if isNullOrUndefined obj
            then success Nothing
            else hyperDecode obj (Just >>> success) failure

-- | `HyperDecode` instance for record type
-- |
-- | `RowToList` is the only type class to relay on to create any instance on Record when we need to access record keys and respective types (which are values in runtime).
-- | Cons data type used to extract type information of a record is not flat, it's kind of a linked list in nature where each node will act as a linked list from rest of the data.
-- | Traversing this everytime when decoding a record will leads to implement a recursion. which can create some deep callStack for records of large size.
-- | To tackle this, we are creating an array representation of the Cons data and using that array representation for decoding.
-- |
instance hyperDecodeRecord :: (FlatRecordDecode row list, RowToList row list) => HyperDecode (Record row) where
    hyperDecode = constructFromIterativeForm constructIterativeData

-- | type for array representation of Cons data,
-- | here `a` is to attach type data, this will be required to infer decodeType when passing this data to a decode function.
-- | `IterativeData a` is equivalent to `Array DecodeEntry` 
data IterativeData a = Type


-- | type that holds decode function for respective key
-- | DecodeEntry is equivalent to [String, Function to decode respective value]
type DecodeEntry = Type

foreign import mkDecodeEntry :: forall fn. String -> fn -> DecodeEntry

class IterativeForm a where
    constructIterativeData :: IterativeData a

instance recordIterativeForm :: (FlatRecordDecode row list, RowToList row list) => IterativeForm (Record row) where
    constructIterativeData = unsafeCoerce (fetchDecoded (Proxy :: Proxy list) [])

class FlatRecordDecode (row :: Row Type) (list :: RowList Type) | list -> row where
    fetchDecoded :: forall p. p list -> Array DecodeEntry -> Array DecodeEntry

instance empty :: FlatRecordDecode () Nil where
    fetchDecoded _ arr = arr

foreign import arrayPush :: forall a. Array a -> a -> Array a

instance nonEmpty :: ( HyperDecode value
                     , FlatRecordDecode rowTail tail
                     , IsSymbol field
                     , Cons field value rowTail row
                     , Lacks field rowTail
                     ) => FlatRecordDecode row (Cons field val tail) where
    fetchDecoded _ arr = fetchDecoded (Proxy :: Proxy tail) $ arrayPush arr (mkDecodeEntry (reflectSymbol (Proxy :: Proxy field)) (hyperDecode :: forall b. Foreign -> (value -> b) -> (String -> b) -> b))

foreign import constructFromIterativeForm :: forall a b. IterativeData a -> Foreign -> (a -> b) -> (String -> b) -> b

-- | an interface to call hyperDecode
decodeForeign :: forall a. (HyperDecode a) => Foreign -> DecodedVal a
decodeForeign obj = hyperDecode obj Val DecodeErr

foreign import tryWithString :: forall a b. String -> (Foreign -> b) -> (String -> b) -> b

-- | an interface to call hyperDecode function on String
decodeString :: forall a. (HyperDecode a) => String -> DecodedVal a
decodeString str = tryWithString str (\x -> hyperDecode x Val DecodeErr) DecodeErr
         
-- | to create a decode instance on any newtype that value have instance of `HyperDecode`
wrapDecode :: forall a b c. (Newtype b a) => (IterativeForm a) => Foreign -> (b -> c) -> (String -> c) -> c
wrapDecode obj success =  constructFromIterativeForm constructIterativeData obj (wrap >>> success)

-- | to create a decode instance on any single value data constructor that value have instance of `HyperDecode`
constructorDecode :: forall a b c. (IterativeForm a) => (a -> b) -> Foreign -> (b -> c) -> (String -> c) -> c
constructorDecode dataConstructor obj success = constructFromIterativeForm constructIterativeData obj (dataConstructor >>> success)