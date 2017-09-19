module Gen where
import qualified Data.ByteString as BS
import Data.Functor.Foldable
import Data.Text.Arbitrary ()
import Test.QuickCheck

import Types

genNVfromNT :: Fix NT -> Gen (Fix NV)
genNVfromNT (Fix a) = Fix <$> go a
  where
    go NInteger = VInteger <$> arbitrary
    go NDouble = VDouble <$> arbitrary
    go NText = VText <$> arbitrary
    go NByteArray = VByteArray <$> (BS.pack <$> listOf arbitrary)
    go NBool = VBool <$> arbitrary
    go NJSON = VJSON <$> arbitrary
    go (NTuple ts) = sized $ \n -> VTuple <$> resize (n `div` length ts) (traverse genNVfromNT ts)
    go (NStruct ts) = sized $ \n -> VStruct <$> resize (n `div` length ts) (traverse (genNVfromNT . snd) ts)
    go (NSum ts) = do n <- choose (0, length ts - 1)
                      let (_, k) = ts !! n
                      VSum n <$> genNVfromNT k
    go (NArray t) = listOf (genNVfromNT t) >>= \l -> return $ VArray (length l) l
    go (NOptional t) = VOptional <$> (arbitrary >>= \case Nothing -> return Nothing
                                                          Just () -> Just <$> genNVfromNT t)
