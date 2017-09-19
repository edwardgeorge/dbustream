{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances.Serial where
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable
import Data.Int
import Data.Text (Text, pack)
import Data.Word
import Test.SmallCheck.Series

import Types

ls :: Monad m => Series m a -> Series m [a]
ls s = x where x = cons0 [] \/ decDepth ((:) <$> s <~> x)

class Monad m => Serial1 m f where
  liftSeries :: Series m a -> Series m (f a)

series1 :: (Serial m a, Serial1 m f) => Series m (f a)
series1 = liftSeries series

instance Serial m a => Serial m (NT a)

instance Monad m => Serial1 m NT where
  liftSeries s = decDepth $ asum [ pure NInteger
                                 , pure NDouble
                                 , pure NText
                                 , pure NBool
                                 , pure NJSON
                                 , NTuple <$> ls s
                                 , NStruct <$> ls ((,) <$> series <~> s)
                                 , NSum <$> ls ((,) <$> series <~> s)
                                 , NArray <$> s
                                 , NOptional <$> s
                                 ]

instance Serial m a => Serial m (NV a)

instance Monad m => Serial1 m NV where
  liftSeries s = decDepth $ asum [ VInteger <$> series
                                 , VDouble <$> series
                                 , VText <$> series
                                 , VByteArray <$> series
                                 , VBool <$> series
                                 , VJSON <$> series
                                 , VTuple <$> ls s
                                 , VStruct <$> ls s
                                 , VSum <$> series <~> s
                                 , VArray <$> series <~> ls s
                                 , VOptional <$> (series >>= traverse (\() -> s))
                                 ]

instance Monad m => Serial m Int64 where
  series = (fromIntegral :: Int -> Int64) <$> series

instance Monad m => Serial m Text where
  series = pack <$> series

instance Monad m => Serial m Word8 where
  series = generate $ \d -> let n = min d $ fromIntegral (maxBound :: Word8)
                            in map fromIntegral [0..n]

instance Monad m => Serial m ByteString where
  series = BS.pack <$> series
