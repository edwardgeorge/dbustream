{-# LANGUAGE TupleSections #-}
import Data.Maybe
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Gen
import Lib
import Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [qcProps]

qcProps :: TestTree
qcProps = testGroup "quickcheck properties"
  [ testProperty "test generation X" (forAll gen $ isJust . uncurry tc)
  , testProperty "round trip" (forAll gen $ \(a, b) -> b == run a b)
  ]

gen = arbitrary >>= \n -> (n,) <$> genNVfromNT n
