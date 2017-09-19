{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances.Arbitrary where
import Control.Monad
import Data.Functor.Foldable
import Test.QuickCheck

import Types

instance Arbitrary1 f => Arbitrary (Fix f) where
  arbitrary = Fix <$> liftArbitrary arbitrary

instance Arbitrary1 NT where
  liftArbitrary g = sized $ \n ->
    if n == 0
    then elements leafs
    else frequency $ map ((1,) . pure) leafs
                ++ [ (2, do k <- choose (1, min 1 n)
                            resize (n `div` k) $ fmap NTuple (replicateM k g))
                   , (2, do k <- choose (1, min 1 n)
                            resize (n `div` k) $ fmap NStruct (replicateM k $ (,) <$> arbitrary <*> g))
                   , (2, do k <- choose (1, min 1 n)
                            resize (n `div` k) $ fmap NSum (replicateM k $ (,) <$> arbitrary <*> g))
                   , (2, NArray <$> g)
                   , (2, NOptional <$> g)
                   ]
    where leafs = [ NInteger , NDouble , NText , NByteArray , NJSON]
