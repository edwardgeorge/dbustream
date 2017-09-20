module Lib where
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS
import Data.Binary.Builder
import Data.Binary.Get
import Data.ByteString.Lazy (ByteString)
import Data.Functor.Foldable
import DBus.Types
import DBus.Wire

import DBus
import Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"

encode :: Fix NT -> Fix NV -> (DBusType, ByteString)
encode fnt fnv = let (x, y)    = serialise (const unfix) fnt fnv
                     (_, _, c) = runRWS y Big 0
                 in (x, toLazyByteString c)

decode :: DBusType -> Fix NT -> ByteString -> Fix NV
decode dbt fnt bs = let g = deserialise Fix dbt fnt
                        h = runReaderT g Big
                    in runGet h bs

run :: Fix NT -> Fix NV -> Fix NV
run a b = let (x, y) = encode a b in decode x a y
