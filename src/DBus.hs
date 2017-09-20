module DBus where
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Align
import qualified Data.Binary.Get as B
import qualified Data.ByteString as BS
import Data.Functor
import Data.Functor.Compose
import Data.Functor.Foldable
import Data.Int
import Data.These
import DBus.Types
import DBus.Wire hiding (getMany)
import GHC.Stack

import Types

host2NV :: HasCallStack => (NT () -> a -> NV a) -> (Fix NT, a) -> X (Fix NT, a)
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
                        (NArray    t, VArray     i) -> X a $ VArray (Fix (NArrayItem 0 t), i)
                        (NArrayItem _ t, VArrayItem i j) -> X a $ VArrayItem (t, i) (Fix (NArrayItem 0 t), j)
                        (NArrayItem _ _, VArrayEnd) -> X a $ VArrayEnd
                        (NOptional t, VOptional  i) -> X a . VOptional $ fmap (t,) i
                        _                           -> error "type mismatch"

serialise :: (NT () -> a -> NV a) -> Fix NT -> a -> (DBusType, DBusPut ())
serialise f nt a = hylo palg (host2NV f) (nt, a)

deserialise :: (NV a -> a) -> DBusType -> Fix NT -> DBusGet a
deserialise f dt nt = hylo (sequenceNV f . getCompose) (Compose . uncurry getCoalg) (dt, nt)

palg :: X (DBusType, DBusPut ()) -> (DBusType, DBusPut ())
palg (X (Fix t) nv) = putAlg (fmap convFixNT t) $ fmap snd nv

putAlg :: HasCallStack => NT DBusType -> NV (DBusPut ()) -> (DBusType, DBusPut ())
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
    go (NArray t)    (VArray      r) = do let al = alignment t
                                          size <- sizeOf 4 al r
                                          putWord32 $ fromIntegral size
                                          alignPut al
                                          r
    go (NArrayItem _ _) (VArrayItem r s) = r >> s
    go (NArrayItem _ _) VArrayEnd        = pure ()
    go (NOptional t) (VOptional   x) = do alignPut 8
                                          case x of
                                            Nothing -> do DBus.Wire.putWord8 0
                                                          -- can't have empty sigs
                                                          putSignatures [DBusSimpleType TypeByte]
                                                          putWord8 0
                                            Just  y -> do DBus.Wire.putWord8 1
                                                          putSignatures [t]
                                                          y
    go _             _               = fail "type mismatch"

getArrayItem :: DBusType -> Int64 -> Fix NT -> DBusGet (NV (DBusType, Fix NT))
getArrayItem s l m = do cur <- fromIntegral <$> lift B.bytesRead
                        case compare cur l of
                          LT -> pure $ VArrayItem (s, m) (s, Fix $ NArrayItem l m)
                          EQ -> pure VArrayEnd
                          GT -> error "too many bytes read for array"

getSimple :: DBusSimpleType -> NT a -> DBusGet (NV b)
getSimple TypeInt64   NInteger = VInteger <$> getInt64
getSimple TypeDouble  NDouble  = VDouble <$> getDouble
getSimple TypeString  NText    = VText <$> getText
getSimple TypeString  NJSON    = VJSON <$> getText
getSimple TypeBoolean NBool    = VBool <$> getBool
getSimple _           _        = fail "simple fail"

getFromArray :: DBusType -> NT (Fix NT) -> DBusGet (NV (DBusType, Fix NT))
getFromArray s n = do
  len <- fromIntegral <$> getWord32
  alignGet (alignment s)
  cur <- fromIntegral <$> lift B.bytesRead
  case (s, n) of
    (_,                       NArray m   ) -> pure $ VArray (s, Fix $ NArrayItem (cur + fromIntegral len) m)
    (DBusSimpleType TypeByte, NByteArray ) -> VByteArray <$> getByteString len
    (t,                       NOptional m) -> case len of
                                                0 -> return $ VOptional Nothing
                                                1 -> return $ VOptional (Just (t, m))
                                                _ -> fail "sdfsdf"
    (_,                       _          ) -> fail "sdfsdf"

getSum :: [(String, Fix NT)] -> DBusGet (NV (DBusType, Fix NT))
getSum m = do con <- fromIntegral <$> DBus.Wire.getWord8
              t <- maybe (fail "") (return . snd) $ listIndex con m
              ss <- getSig
              return $ VSum con (ss, t)

getOptional :: Fix NT -> DBusGet (NV (DBusType, Fix NT))
getOptional m = do inh <- getWord8
                   ss <- getSig
                   if inh > 0
                     then return $ VOptional (Just (ss, m))
                     else if ss /= DBusSimpleType TypeByte
                          then fail "grrehfefg"
                          else getWord8 $> VOptional Nothing

getCoalg :: HasCallStack => DBusType -> Fix NT -> DBusGet (NV (DBusType, Fix NT))
getCoalg s             (Fix (NArrayItem l m)) = getArrayItem s l m
getCoalg (DBusSimpleType s) (Fix n) = getSimple s n
getCoalg (TypeArray s) (Fix n) = getFromArray s n
getCoalg (TypeStruct [DBusSimpleType s, TypeVariant]) (Fix n) = do
  alignGet 8
  case (s, n) of
    (TypeByte,    NSum m     ) -> getSum m
    (TypeBoolean, NOptional m) -> getOptional m
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
  VArray           i -> f . VArray <$> i
  VArrayItem     i j -> f <$> liftM2 VArrayItem i j
  VArrayEnd          -> pure $ f VArrayEnd
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
convNT (NArrayItem _ t) = t
convNT (NOptional _) = TypeStruct [DBusSimpleType TypeBoolean, TypeVariant]

getSig :: HasCallStack => DBusGet DBusType
getSig = do ss <- getSignatures
            case ss of
              [s] -> return s
              _   -> fail ""

listIndex :: Int -> [a] -> Maybe a
listIndex _ [] = Nothing
listIndex 0 (x:_) = Just x
listIndex n (_:xs) = listIndex (n - 1) xs
