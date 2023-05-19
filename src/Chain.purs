module Chain where

import Prelude
import Foreign (Foreign, unsafeFromForeign)
import Type.RowList (class RowToList, RowList, Nil, Cons)
import Type.Proxy (Proxy(Proxy))
import LiteDecode.Decode (lookupVal, unsafeInsert)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons, class Lacks)
import Data.Maybe (Maybe, Maybe(Just), Maybe(Nothing))
import Main.DecodeError (DecodedVal, DecodedVal(Val), DecodedVal(DecodeErr))
import Foreign.Object (Object)
import Foreign.Object (mapWithKey) as Object
import Foreign.Generic.Internal (isObject)
import Data.Newtype (class Newtype, wrap)
import Data.Function.Uncurried (Fn3, runFn3)

class ChainDecode a where
    chainDecode :: forall b. Foreign -> (a -> b) -> (String -> b) -> b

foreign import stringDecodeImpl :: forall b. Fn3 Foreign  (String -> b)  (String -> b) b

instance stringDecode :: ChainDecode String where
    chainDecode = runFn3 stringDecodeImpl

foreign import intDecodeImpl :: forall b. Foreign -> (Int -> b) -> (String -> b) -> b

instance intDecode :: ChainDecode Int where
    chainDecode = intDecodeImpl

foreign import numDecodeImpl :: forall b. Foreign -> (Number -> b) -> (String -> b) -> b

instance numberDecode :: ChainDecode Number where
    chainDecode = numDecodeImpl

foreign import bitDecodeImpl :: forall b. Foreign -> (Boolean -> b) -> (String -> b) -> b

instance bitDecode :: ChainDecode Boolean where
    chainDecode = bitDecodeImpl

instance foreignDecode :: ChainDecode Foreign where
    chainDecode obj success _ = success obj

instance objectDecode :: (ChainDecode a) => ChainDecode (Object a) where
    chainDecode obj success failure = if isObject obj
        then tryCatch obj (\frn -> Object.mapWithKey (\_ -> (\x -> chainDecode x rowSuccess shortCircuit)) (unsafeFromForeign frn)) success failure
        else failure "not an object"

foreign import arrDecodeImpl :: forall a b c. Foreign -> (Foreign -> a) -> (Array a -> c) -> (String -> c) -> c

foreign import shortCircuit :: forall a. String -> a

rowSuccess :: forall a.a -> a
rowSuccess x = x

instance arrDecode :: (ChainDecode a) => ChainDecode (Array a) where
    chainDecode obj success failure = arrDecodeImpl obj (\x -> chainDecode x (\y -> y) shortCircuit) success failure

foreign import maybeDecodeImpl :: forall a b. Foreign -> (String -> b) -> Maybe a -> (Foreign -> b) -> b

instance maybeDecode :: (ChainDecode a) => ChainDecode (Maybe a) where
    chainDecode obj success failure = maybeDecodeImpl obj failure Nothing decodeVal
        where
        decodeVal = \x -> chainDecode x (success <<< Just) failure

foreign import tryCatch :: forall a b. Foreign -> (Foreign -> a) -> (a -> b) -> (String -> b) -> b

instance recordDecodeChain :: (RecordDecode row list, RowToList row list) => ChainDecode (Record row) where
    chainDecode obj success failure = tryCatch obj (recordDecode (Proxy :: Proxy list)) success failure

class RecordDecode (row :: Row Type) (list :: RowList Type) | list -> row where
    recordDecode :: forall p. p list -> Foreign -> (Record row)

instance emptyRecordDecode :: RecordDecode () Nil where
    recordDecode _ obj = {}

foreign import storeSomewhere :: forall a. a -> a

instance nonEmptyRecordDecode :: ( ChainDecode value
                               , RecordDecode rowTail tail
                               , IsSymbol field
                               , Cons field value rowTail row
                               , Lacks field rowTail
                               ) => RecordDecode row (Cons field val tail) where
    recordDecode _ obj =
        unsafeInsert (Proxy :: Proxy field) (chainDecode val rowSuccess shortCircuit) (recordDecode (Proxy :: Proxy tail) obj)
        where
        val = lookupVal obj (reflectSymbol (Proxy :: Proxy field))

decodeForeign :: forall a. (ChainDecode a) => Foreign -> DecodedVal a
decodeForeign obj = chainDecode obj Val DecodeErr

foreign import tryWithString :: forall a b. String -> (Foreign -> b) -> (String -> b) -> b

decodeString :: forall a. (ChainDecode a) => String -> DecodedVal a
decodeString str = tryWithString str (\x -> chainDecode x Val DecodeErr) DecodeErr


wrapDecode :: forall a b c. (Newtype b a) => (ChainDecode a) => Foreign -> (b -> c) -> (String -> c) -> c
wrapDecode obj success failure = case decodeForeign obj :: DecodedVal a of
    DecodeErr err -> failure err
    Val       val -> success $ wrap val

constructorDecode :: forall a b c. (ChainDecode a) => (a -> b) -> Foreign -> (b -> c) -> (String -> c) -> c
constructorDecode l obj success failure = case decodeForeign obj :: DecodedVal a of
    DecodeErr err -> failure err
    Val       val -> success $ l val