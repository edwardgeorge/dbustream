module DBus where
import Control.Monad
import Data.Align
import qualified Data.ByteString as BS
import Data.Functor.Compose
import Data.Functor.Foldable
import Data.These
import DBus.Types
import DBus.Wire

import Types

data X a = X (Fix NT) (NV a) deriving (Functor)

host2NV :: (NT () -> a -> NV a) -> (Fix NT, a) -> X (Fix NT, a)
host2NV f (a, b) = let a' = unfix a
                       nt = void a'
                   in case (a', f nt b) of
                        (NInteger,    VInteger   i) -> X a $ VInteger i
                        (NDouble,     VDouble    i) -> X a $ VDouble i
                        (NText,       VText      i) -> X a $ VText i
                        (NByteArray,  VByteArray i) -> X a $ VByteArray i
                        (NBool,       VBool      i) -> X a $ VBool i
                        (NJSON,       VJSON      i) -> X a $ VJSON i
                        (NTuple    t, VTuple     i) -> X a . VTuple $ zip t i -- TODO: check length?
                        (NStruct   t, VStruct    i) -> X a . VStruct $ zip (fmap snd t) i -- TODO: check length?
                        (NSum      t, VSum     c i) -> X a $ VSum c (snd $ t !! c, i)
                        (NArray    t, VArray   l i) -> X a . VArray l $ fmap (t,) i
                        (NOptional t, VOptional  i) -> X a . VOptional $ fmap (t,) i
                        _                           -> error "type mismatch"

serialise :: (NT () -> a -> NV a) -> Fix NT -> a -> (DBusType, DBusPut ())
serialise f nt a = hylo palg (host2NV f) (nt, a)

deserialise :: (NV a -> a) -> DBusType -> Fix NT -> DBusGet a
deserialise f dt nt = hylo (sequenceNV f . getCompose) (Compose . uncurry getCoalg) (dt, nt)

palg :: X (DBusType, DBusPut ()) -> (DBusType, DBusPut ())
palg (X (Fix t) nv) = putAlg (fmap convFixNT t) $ fmap snd nv

putAlg :: NT DBusType -> NV (DBusPut ()) -> (DBusType, DBusPut ())
putAlg t' v = (convNT t', go t' v)
  where
    go _             (VInteger    i) = putInt64 i
    go _             (VDouble     d) = putDouble d
    go _             (VText       t) = putText t
    go _             (VByteArray  b) = putWord32 (fromIntegral $ BS.length b) >> putByteString b
    go _             (VBool       b) = putWord32 $ fromEnum' b
    go _             (VJSON       t) = putText t
    go _             (VTuple      r) = alignPut 8 >> sequence_ r
    go _             (VStruct     r) = alignPut 8 >> sequence_ r
    go (NSum t)      (VSum      c r) = do alignPut 8
                                          DBus.Wire.putWord8 (fromIntegral c)
                                          putSignatures [snd $ t !! c]
                                          r
    go (NArray t)    (VArray    _ r) = do let al = alignment t
                                              content = sequence_ r
                                          size <- sizeOf 4 al content
                                          putWord32 $ fromIntegral size
                                          alignPut al
                                          content
    go (NOptional t) (VOptional   x) = do alignPut 8
                                          case x of
                                            Nothing -> do DBus.Wire.putWord8 0
                                                          putSignatures []
                                            Just  y -> do DBus.Wire.putWord8 1
                                                          putSignatures [t]
                                                          y
    go _             _               = fail "type mismatch"

getCoalg :: DBusType -> Fix NT -> DBusGet (NV (DBusType, Fix NT))
getCoalg (DBusSimpleType s) (Fix n) = case (s, n) of
  (TypeInt64,   NInteger) -> VInteger <$> getInt64
  (TypeDouble,  NDouble)  -> VDouble <$> getDouble
  (TypeString,  NText)    -> VText <$> getText
  (TypeString,  NJSON)    -> VJSON <$> getText
  (TypeBoolean, NBool)    -> VBool <$> getBool
  _                       -> fail ""
getCoalg (TypeArray s) (Fix n) = do
  len <- fromIntegral <$> getWord32
  case (s, n) of
    (_,                       NArray m   ) -> return . VArray len $ replicate len (s, m)
    (DBusSimpleType TypeByte, NByteArray ) -> VByteArray <$> getByteString len
    (t,                       NOptional m) -> case len of
                                                0 -> return $ VOptional Nothing
                                                1 -> return $ VOptional (Just (t, m))
                                                _ -> fail ""
    (_,                       _          ) -> fail ""
getCoalg (TypeStruct [DBusSimpleType s, TypeVariant]) (Fix n) = do
  alignGet 8
  case (s, n) of
    (TypeByte,    NSum m     ) -> do con <- fromIntegral <$> DBus.Wire.getWord8
                                     t <- maybe (fail "") (return . snd) $ listIndex con m
                                     ss <- getSig
                                     return $ VSum con (ss, t)
    (TypeBoolean, NOptional m) -> do inh <- getBool
                                     if inh
                                       then do ss <- getSig
                                               return $ VOptional (Just (ss, m))
                                       else return $ VOptional Nothing
    (_,           _          ) -> fail ""
getCoalg (TypeStruct s) (Fix n) = do
  alignGet 8
  (cons, ts) <- case n of
                  NStruct m -> return (VStruct, fmap snd m)
                  NTuple  m -> return (VTuple, m)
                  _         -> fail ""
  case traverse justThese $ align s ts of
    Nothing -> fail ""
    Just xs -> return $ cons xs
getCoalg TypeVariant n = do
  ss <- getSig
  getCoalg ss n
getCoalg _ _ = fail ""

sequenceNV :: Monad m => (NV a -> a) -> m (NV (m a)) -> m a
sequenceNV f m = m >>= \case
  VInteger         i -> return . f $ VInteger i
  VDouble          i -> return . f $ VDouble i
  VText            i -> return . f $ VText i
  VByteArray       i -> return . f $ VByteArray i
  VBool            i -> return . f $ VBool i
  VJSON            i -> return . f $ VJSON i
  VTuple           i -> f . VTuple <$> sequence i
  VStruct          i -> f . VStruct <$> sequence i
  VSum           c i -> f . VSum c <$> i
  VArray         l i -> f . VArray l <$> sequence i
  VOptional  Nothing -> return . f $ VOptional Nothing
  VOptional (Just i) -> f . VOptional . Just <$> i

convFixNT :: Fix NT -> DBusType
convFixNT = cata convNT

convNT :: NT DBusType -> DBusType
convNT  NInteger     = DBusSimpleType TypeInt64
convNT  NDouble      = DBusSimpleType TypeDouble
convNT  NText        = DBusSimpleType TypeString
convNT  NByteArray   = TypeArray (DBusSimpleType TypeByte)
convNT  NBool        = DBusSimpleType TypeBoolean
convNT  NJSON        = DBusSimpleType TypeString
convNT (NTuple   rs) = TypeStruct rs
convNT (NStruct  rs) = TypeStruct $ fmap snd rs
convNT (NSum      _) = TypeStruct [DBusSimpleType TypeByte, TypeVariant]
convNT (NArray    t) = TypeArray t
convNT (NOptional _) = TypeStruct [DBusSimpleType TypeBoolean, TypeVariant]

getSig :: DBusGet DBusType
getSig = do ss <- getSignatures
            case ss of
              [s] -> return s
              _   -> fail ""

listIndex :: Int -> [a] -> Maybe a
listIndex _ [] = Nothing
listIndex 0 (x:_) = Just x
listIndex n (_:xs) = listIndex (n - 1) xs
