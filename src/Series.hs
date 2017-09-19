module Series where
import Control.Monad
import Data.Functor.Foldable
import Test.SmallCheck.Series

import Instances.Serial ()
import Types

seriesNVfromNT :: Monad m => Fix NT -> Series m (Fix NV)
seriesNVfromNT (Fix a) = Fix <$> go a
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
    go (NArray    t) = do n <- getDepth
                          m <- generate $ const [0..n]
                          VArray m <$> replicateM m (seriesNVfromNT t)
    go (NOptional t) = VOptional <$> (series >>= traverse (\() -> seriesNVfromNT t))
