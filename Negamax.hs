{-# LANGUAGE FlexibleInstances #-}

module Negamax
(
        ExtendedNum (..),
        NegamaxTree (..),
        evaluate
) where


data ExtendedNum a = Only a | NegInf | PosInf deriving (Eq, Show)

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
        Only a * NegInf
                | a == 0 = Only 0
                | signum a == 1 = NegInf
                | signum a == -1 = PosInf
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

instance (Ord a) => Ord (ExtendedNum a) where
        Only a <= Only b = a <= b
        NegInf <= Only a = True
        Only a <= NegInf = False
        PosInf <= Only a = False
        Only a <= PosInf = True
        NegInf <= PosInf = True
        PosInf <= NegInf = False
        PosInf <= PosInf = True
        NegInf <= NegInf = True

instance Functor ExtendedNum where
        fmap f (Only a) = Only (f a)
        fmap f NegInf = NegInf
        fmap f PosInf = PosInf

data NegamaxTree state = EmptyTree | Node state [NegamaxTree state] deriving Show

instance Functor NegamaxTree where
        fmap f (Node a []) = Node (f a) []
        fmap f (Node a xs) = Node (f a) (map (fmap f) xs)
        fmap f EmptyTree = EmptyTree

extendedNum2Num :: (Num a) => ExtendedNum a -> a
extendedNum2Num NegInf = error "Things went wrong!"
extendedNum2Num PosInf = error "Things went wrong!"
extendedNum2Num (Only x) = x

evaluate :: NegamaxTree state -> (state -> ExtendedNum Integer) -> Int -> ExtendedNum Integer
evaluate (Node state [] ) evalFunc _ = evalFunc state
evaluate (Node state _ ) evalFunc 0 = evalFunc state
evaluate (Node state xs) evalFunc depth = Only (-1) * minimum (map (evaluateOutOfOrder evalFunc (depth - 1)) xs) where
        evaluateOutOfOrder = \x y z -> evaluate z x y
