{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module DecodedVal where

import Prelude
import Data.Show (class Show)
import Control.Alt (class Alt)
import Data.Maybe (Maybe, Maybe(Nothing), Maybe(Just))

data DecodedVal a = DecodeErr String | Val a

instance showDecodedVal :: (Show a) => Show (DecodedVal a) where
    show (DecodeErr x) = x
    show (Val       v) = show v

instance functorDecodedVal :: Functor DecodedVal where
    map fun (DecodeErr x) = DecodeErr x
    map fun (Val       v) = Val $ fun v

instance altDecodedVal :: Alt DecodedVal where
    alt (Val val) _ = Val val
    alt _         (Val val) = Val val
    alt _         x = x

hush' :: forall a. DecodedVal a -> Maybe a
hush' val = case val of
    Val v -> Just v
    DecodeErr err -> Nothing
