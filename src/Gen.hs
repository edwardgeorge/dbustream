module Gen ( module Gen
           , module Internal.Gen
           ) where
import Data.Functor.Foldable
import Data.Text.Arbitrary ()
import Test.QuickCheck

import Internal.Gen
import Instances.Arbitrary ()
import Types

genXfromNT :: Fix NT -> Gen (Fix X)
genXfromNT = go where go a = Fix . X a <$> genNVfromNT' go a

genX :: Gen (Fix X)
genX = arbitrary >>= genXfromNT
