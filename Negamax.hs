{-# LANGUAGE FlexibleInstances #-}

module Negamax
(
        ExtendedNum (..),
        NegamaxTree (..),
        evaluate,
        generateNegamaxTree
) where


data ExtendedNum a = Only a | NegInf | PosInf deriving (Eq, Show)

instance Num (ExtendedNum Integer) where
        Only a + Only b = Only (a + b)
        Only a + NegInf = NegInf
        NegInf + Only a = NegInf
        Only a + PosInf = PosInf
        PosInf + Only a = PosInf

        x - y = x + Only (-1) * y

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

data NegamaxTree state = EmptyTree | Node state [NegamaxTree state] deriving (Eq)

instance Functor NegamaxTree where
        fmap f (Node a []) = Node (f a) []
        fmap f (Node a xs) = Node (f a) (map (fmap f) xs)
        fmap f EmptyTree = EmptyTree

instance (Show a) => Show (NegamaxTree a) where
        show = flip showHelperTree 0

indentNTimes :: Int -> String -> String
indentNTimes n xs = iterate ((++) "  ") xs !! n

showHelperTree :: (Show a) => NegamaxTree a -> Int -> String
-- Helper function for show function
showHelperTree EmptyTree n = indentNTimes n "EmptyTree"
showHelperTree tree@(Node state xs) n
        | null xs = indentNTimes n baseString
        | otherwise = indentNTimes n baseString ++ foldl (++) "" restOfTree
        where baseString = "| " ++ show state ++ "\n"
              restOfTree = map (flip showHelperTree (n + 1)) xs

extendedNum2Num :: (Num a) => ExtendedNum a -> a
extendedNum2Num NegInf = error "Things went wrong!"
extendedNum2Num PosInf = error "Things went wrong!"
extendedNum2Num (Only x) = x

evaluate :: NegamaxTree state -> (state -> ExtendedNum Integer) -> Int -> ExtendedNum Integer
evaluate (Node state [] ) evalFunc _ = evalFunc state
evaluate (Node state _ ) evalFunc 0 = evalFunc state
evaluate (Node state xs) evalFunc depth = Only (-1) * minimum (map (evaluateOutOfOrder evalFunc (depth - 1)) xs) where
        evaluateOutOfOrder = \x y z -> evaluate z x y

generateNegamaxTree :: state -> (move -> state -> state)  -> (state -> Bool) -> (state -> [move]) -> NegamaxTree state
generateNegamaxTree state moveFunc checkGameOverFunc generatePossibleMovesFunc
        | checkGameOverFunc state = Node state []
        | otherwise = Node state listOfNodes
                where listOfNodes = map (argsModifiedGenerateNegamaxTree moveFunc checkGameOverFunc generatePossibleMovesFunc . (flip moveFunc state)) validMoves
                      argsModifiedGenerateNegamaxTree = \a b c d -> generateNegamaxTree d a b c
                      validMoves = generatePossibleMovesFunc state
