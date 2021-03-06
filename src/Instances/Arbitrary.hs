{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances.Arbitrary where
import Control.Monad
import Data.Functor.Foldable
import Test.QuickCheck

import Internal.Gen
import Types

instance Arbitrary1 f => Arbitrary (Fix f) where
  arbitrary = Fix <$> liftArbitrary arbitrary

instance Arbitrary1 NT where
  liftArbitrary g = sized $ \n ->
    if n <= 0
    then elements leafs
    else frequency $ map ((18,) . pure) leafs
                ++ [ (22, do k <- choose (1, min 1 n)
                             resize (n `div` k) $ fmap NTuple (replicateM k g))
                   , (22, do k <- choose (1, min 1 n)
                             resize (n `div` k) $ fmap NStruct (replicateM k $ (,) <$> arbitrary <*> g))
                   , (22, do k <- choose (1, min 1 n)
                             resize (n `div` k) $ fmap NSum (replicateM k $ (,) <$> arbitrary <*> g))
                   , (22, NArray <$> g)
                   , (22, NOptional <$> g)
                   ]
    where leafs = [ NInteger , NDouble , NText , NByteArray , NJSON]

instance Arbitrary1 X where
  liftArbitrary g = arbitrary >>= \nt -> X nt <$> genNVfromNT' (const g) nt
