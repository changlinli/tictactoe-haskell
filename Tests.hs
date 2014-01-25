{-# LANGUAGE FlexibleInstances #-}

import Data.Char
import Test.QuickCheck
import Negamax

{-instance Arbitrary (ExtendedNum Integer) where-}
        {-arbitrary :: Arbitrary a => Gen a-}
        {-arbitrary = fmap fromInteger (choose (0, 1000000000000000000)) :: ExtendedNum Integer-}
        {-coarbitrary x = variant (ord x `rem` 4)-}

prop_AbsSignum :: ExtendedNum Integer -> Bool
prop_AbsSignum x = abs x * signum x == x
