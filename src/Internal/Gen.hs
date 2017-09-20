module Internal.Gen where
import qualified Data.ByteString as BS
import Data.Functor.Foldable
import Data.Text.Arbitrary ()
import Test.QuickCheck

import Types

genNVfromNT :: Fix NT -> Gen (Fix NV)
genNVfromNT = go where go a = Fix <$> genNVfromNT' go a

genNVfromNT' :: (Fix NT -> Gen a) -> Fix NT -> Gen (NV a)
genNVfromNT' g (Fix a) = go a
  where
    go  NInteger     = VInteger <$> arbitrary
    go  NDouble      = VDouble <$> arbitrary
    go  NText        = VText <$> arbitrary
    go  NByteArray   = VByteArray <$> (BS.pack <$> listOf arbitrary)
    go  NBool        = VBool <$> arbitrary
    go  NJSON        = VJSON <$> arbitrary
    go (NTuple   ts) = sized $ \n -> VTuple <$> resize (n `div` length ts) (traverse g ts)
    go (NStruct  ts) = sized $ \n -> VStruct <$> resize (n `div` length ts) (traverse (g . snd) ts)
    go (NSum     ts) = do n <- choose (0, length ts - 1)
                          let (_, k) = ts !! n
                          VSum n <$> g k
    -- go (NArray    t) = listOf (g t) >>= \l -> return $ VArray (length l) l
    go (NArray    t) = VArray <$> g (Fix $ NArrayItem 0 t)
    go (NArrayItem _ t) = sized $ \n -> do k <- choose (0, n)
                                           if k == 0
                                             then pure VArrayEnd
                                             else newArrayItem g t
    go (NOptional t) = VOptional <$> (arbitrary >>= \case Nothing -> return Nothing
                                                          Just () -> Just <$> g t)

newArrayItem :: (Fix NT -> Gen a) -> Fix NT -> Gen (NV a)
newArrayItem g t = g t >>= \a -> g (Fix $ NArrayItem 0 t) >>= \b -> return $ VArrayItem a b
