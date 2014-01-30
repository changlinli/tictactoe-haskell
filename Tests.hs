{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.Framework
import Data.Char
import Test.QuickCheck
import Negamax

instance Arbitrary (ExtendedNum Integer) where
        arbitrary = oneof [integerGen, infGen] where
                integerGen = fmap Only (choose (0 :: Integer, 1000000000000000000 :: Integer))
                infGen = elements [PosInf, NegInf]

prop_AbsSignum :: ExtendedNum Integer -> Bool
prop_AbsSignum x = abs x * signum x == x

main = htfMain htf_thisModulesTests
