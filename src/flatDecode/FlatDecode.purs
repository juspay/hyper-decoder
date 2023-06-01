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

foreign import unsafeInsertImpl :: forall a r1 r2. String -> a -> Record r1 -> Record r2

foreign import tryCatch :: forall a b. Foreign -> (Foreign -> a) -> (a -> b) -> (String -> b) -> b

class HyperDecode a where
    hyperDecode :: forall b. Foreign -> (a -> b) -> (String -> b) -> b

foreign import primitiveDecodeImpl :: forall a b. String -> Foreign -> (a -> b) -> (String -> b) -> b

instance stringDecode :: HyperDecode String where
    hyperDecode = primitiveDecodeImpl "string"

instance intDecode :: HyperDecode Int where
    hyperDecode = primitiveDecodeImpl "number"

instance numberDecode :: HyperDecode Number where
    hyperDecode = primitiveDecodeImpl "number"

instance bitDecode :: HyperDecode Boolean where
    hyperDecode = primitiveDecodeImpl "boolean"

instance foreignDecode :: HyperDecode Foreign where
    hyperDecode obj success _ = success obj

instance objectDecode :: (HyperDecode a) => HyperDecode (Object a) where
    hyperDecode obj success failure = if isObject obj
        then tryCatch obj (\frn -> Object.mapWithKey (\_ -> (\x -> hyperDecode x rowSuccess shortCircuit)) (unsafeFromForeign frn)) success failure
        else failure "not an object"

foreign import arrDecodeImpl :: forall a b c. Foreign -> (Foreign -> a) -> (Array a -> c) -> (String -> c) -> c

foreign import shortCircuit :: forall a. String -> a

rowSuccess :: forall a.a -> a
rowSuccess x = x

instance arrDecode :: (HyperDecode a) => HyperDecode (Array a) where
    hyperDecode obj success failure = arrDecodeImpl obj (\x -> hyperDecode x (\y -> y) shortCircuit) success failure

foreign import maybeDecodeImpl :: forall a b. Foreign -> (String -> b) -> Maybe a -> (Foreign -> b) -> b

instance maybeDecode :: (HyperDecode a) => HyperDecode (Maybe a) where
    hyperDecode obj success failure = maybeDecodeImpl obj failure Nothing decodeVal
        where
        decodeVal = \x -> hyperDecode x (success <<< Just) failure

instance hyperDecodeRecord :: (FlatRecordDecode row list, RowToList row list) => HyperDecode (Record row) where
    hyperDecode = constructFromIterativeForm constructIterativeData

data IterativeData a = Type

foreign import mkDecodeEntry :: forall fn. String -> fn -> DecodeEntry

type DecodeEntry = Type

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

foreign import constructNewTypeFromIterativeForm :: forall a b c. IterativeData a -> (a -> b) -> Foreign -> (b -> c) -> (String -> c) -> c

foreign import constructFromIterativeForm :: forall a b. IterativeData a -> Foreign -> (a -> b) -> (String -> b) -> b

decodeForeign :: forall a. (HyperDecode a) => Foreign -> DecodedVal a
decodeForeign obj = hyperDecode obj Val DecodeErr

foreign import tryWithString :: forall a b. String -> (Foreign -> b) -> (String -> b) -> b

decodeString :: forall a. (HyperDecode a) => String -> DecodedVal a
decodeString str = tryWithString str (\x -> hyperDecode x Val DecodeErr) DecodeErr
         

wrapDecode :: forall a b c. (Newtype b a) => (IterativeForm a) => Foreign -> (b -> c) -> (String -> c) -> c
wrapDecode =  constructNewTypeFromIterativeForm constructIterativeData wrap

constructorDecode :: forall a b c. (IterativeForm a) => (a -> b) -> Foreign -> (b -> c) -> (String -> c) -> c
constructorDecode l = constructNewTypeFromIterativeForm constructIterativeData l