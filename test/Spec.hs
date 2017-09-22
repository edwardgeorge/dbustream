{-# LANGUAGE TupleSections #-}
import Data.Functor.Foldable
import Data.Maybe
import Test.Tasty
import Test.SmallCheck.Series (series, Series)
import qualified Test.Tasty.SmallCheck as S
import qualified Test.Tasty.QuickCheck as Q
import Test.QuickCheck

import Gen
import Lib
import Series
import Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [qcProps, scProps]

qcProps :: TestTree
qcProps = testGroup "quickcheck properties"
  [ Q.testProperty "test generation X" (Q.forAll gen $ isJust . uncurry tc)
  , Q.testProperty "round trip" (Q.forAll gen $ \(a, b) -> b == run a b)
  ]

scProps :: TestTree
scProps = testGroup "smallcheck properties"
  [ S.testProperty "test generation X" $ S.forAll (S.over ser $ isJust . uncurry tc)
  , S.testProperty "round trip" $ S.forAll (S.over ser $ \(a, b) -> b == run a b)
  ]

gen :: Gen (Fix NT, Fix NV)
gen = arbitrary >>= \n -> (n,) <$> genNVfromNT n
ser :: Monad m => Series m (Fix NT, Fix NV)
ser = series >>= \n -> (n,) <$> seriesNVfromNT n
