module Series where
import Control.Monad
import Data.Functor.Foldable
import Test.SmallCheck.Series

import Instances.Serial ()
import Types

seriesNVfromNT :: Monad m => Fix NT -> Series m (Fix NV)
seriesNVfromNT (Fix a) = Fix <$> decDepth (go a)
  where
    go  NInteger     = VInteger <$> series
    go  NDouble      = VDouble <$> series
    go  NText        = VText <$> series
    go  NByteArray   = VByteArray <$> series
    go  NBool        = VBool <$> series
    go  NJSON        = VJSON <$> series
    go (NTuple   ts) = VTuple <$> traverse seriesNVfromNT ts
    go (NStruct  ts) = VStruct <$> traverse (seriesNVfromNT . snd) ts
    go (NSum     ts) = generate (const $ zip [0..] ts) >>= \(i, (_, t)) -> VSum i <$> seriesNVfromNT t
    go (NArray    t) = VArray <$> seriesNVfromNT (Fix $ NArrayItem 0 t)
    go (NArrayItem _ t) = do n <- getDepth
                             if n <= 0
                               then pure VArrayEnd
                               else cons0 VArrayEnd \/ (VArrayItem
                                                        <$> seriesNVfromNT t
                                                        <~> seriesNVfromNT (Fix $ NArrayItem 0 t))
    go (NOptional t) = VOptional <$> (series >>= traverse (\() -> seriesNVfromNT t))
