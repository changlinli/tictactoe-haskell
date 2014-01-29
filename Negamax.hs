{-# LANGUAGE FlexibleInstances #-}

module Negamax
(
        ExtendedNum (..)
) where

data ExtendedNum a = Only a | NegInf | PosInf deriving (Eq, Show)
data NegamaxTree a state = EmptyTree | Node a state [NegamaxTree a state]

instance Num (ExtendedNum Integer) where
        Only a + Only b = Only (a + b)
        Only a + NegInf = NegInf
        NegInf + Only a = NegInf
        Only a + PosInf = PosInf
        PosInf + Only a = PosInf

        x - y = ((signum x) * x) + ((signum y) * y)

        Only a * Only b = Only (a * b)
        Only a * PosInf
                | a == 0 = Only 0
                | signum a == 1 = PosInf
                | signum a == -1 = NegInf
        Only a * PosInf
                | a == 0 = Only 0
                | signum a == 1 = PosInf
                | signum a == -1 = NegInf
        PosInf * PosInf = PosInf
        NegInf * NegInf = PosInf
        PosInf * NegInf = NegInf
        NegInf * x = x * NegInf
        PosInf * x = x * PosInf

        abs (Only a) = Only (abs a)
        abs NegInf = PosInf
        abs PosInf = PosInf
        
        signum (Only a) = Only (signum a)
        signum PosInf = Only 1
        signum NegInf = Only (-1)

        fromInteger a = Only a

instance Functor ExtendedNum where
        fmap f (Only a) = Only (f a)
        fmap f NegInf = NegInf
        fmap f PosInf = PosInf

singleton :: a -> state -> NegamaxTree a state
singleton x state = Node x state [ EmptyTree ]

extendedNum2Num :: (Num a) => ExtendedNum a -> a
extendedNum2Num NegInf = error "Things went wrong!"
extendedNum2Num PosInf = error "Things went wrong!"
extendedNum2Num (Only x) = x

evaluateState :: (state -> ExtendedNum Integer) -> state -> ExtendedNum Integer
evaluateState f state = f state

evaluateNode :: (state -> ExtendedNum Integer)
evaluateNode state = Only 0

evaluateTree :: NegamaxTree a state -> state
evaluateTree (Node a state []) = state
