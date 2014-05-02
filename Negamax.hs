{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Negamax
(
        ExtendedNum (..),
        NegamaxTree (..),
        evaluate,
        generateNegamaxTree,
        findBestMove,
        extendedNum2Num
) where


data ExtendedNum a = Only a | NegInf | PosInf deriving (Eq, Show)

instance (Num a, Eq a) => Num (ExtendedNum a) where
        Only a + Only b = Only (a + b)
        Only _ + NegInf = NegInf
        Only _ + PosInf = PosInf
        NegInf + NegInf = NegInf
        PosInf + PosInf = PosInf
        NegInf + PosInf = 0
        x + y = y + x

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
        x * y = y * x

        abs (Only a) = Only (abs a)
        abs NegInf = PosInf
        abs PosInf = PosInf
        
        signum (Only a) = Only (signum a)
        signum PosInf = Only 1
        signum NegInf = Only (-1)

        fromInteger a = Only $ fromInteger a

instance (Ord a) => Ord (ExtendedNum a) where
        Only a <= Only b = a <= b
        NegInf <= Only _ = True
        Only _ <= NegInf = False
        PosInf <= Only _ = False
        Only _ <= PosInf = True
        NegInf <= PosInf = True
        PosInf <= NegInf = False
        PosInf <= PosInf = True
        NegInf <= NegInf = True

instance Functor ExtendedNum where
        fmap f (Only a) = Only (f a)
        fmap _ NegInf = NegInf
        fmap _ PosInf = PosInf

data NegamaxTree state = Node state [NegamaxTree state] deriving (Eq)

instance Functor NegamaxTree where
        fmap f (Node a []) = Node (f a) []
        fmap f (Node a xs) = Node (f a) (map (fmap f) xs)

instance (Show a) => Show (NegamaxTree a) where
        show = flip showHelperTree 0

indentNTimes :: Int -> String -> String
indentNTimes n xs = iterate ((++) "  ") xs !! n

showHelperTree :: (Show a) => NegamaxTree a -> Int -> String
-- Helper function for show function
showHelperTree (Node state xs) n
        | null xs = indentNTimes n baseString
        | otherwise = indentNTimes n baseString ++ foldl (++) "" restOfTree
        where baseString = "| " ++ show state ++ "\n"
              restOfTree = map (flip showHelperTree (n + 1)) xs

extendedNum2Num :: (Num a) => ExtendedNum a -> a
extendedNum2Num NegInf = error "Things went wrong!"
extendedNum2Num PosInf = error "Things went wrong!"
extendedNum2Num (Only x) = x

evaluateAB :: NegamaxTree state -> (state -> ExtendedNum Integer) -> Int -> ExtendedNum Integer
evaluateAB stateTree evalFunc depth = alphaBetaHelper stateTree evalFunc depth NegInf PosInf

alphaBetaHelper :: NegamaxTree state -> (state -> ExtendedNum Integer) -> Int -> ExtendedNum Integer -> ExtendedNum Integer -> ExtendedNum Integer
alphaBetaHelper (Node state []) evalFunc _ _ _ = evalFunc state
alphaBetaHelper (Node state _ ) evalFunc 0 _ _ = evalFunc state
alphaBetaHelper (Node _ xs) evalFunc depth alpha beta = (-1) * fst bestMoveBetaPair where
        bestMoveBetaPair = foldl accValueBeta startingPair xs
        startingPair = (PosInf, beta)
        accValueBeta pair@(value, accBeta) childNode
                | accBeta <= alpha = pair
                | otherwise = (bestValue, newBeta)
                where bestValue = min value newValue
                      newValue = alphaBetaHelper childNode evalFunc (depth - 1) ((-1) * beta) ((-1) * alpha)
                      newBeta = min accBeta newValue

evaluate :: NegamaxTree state -> (state -> ExtendedNum Integer) -> Int -> ExtendedNum Integer
evaluate (Node state [] ) evalFunc _ = evalFunc state
evaluate (Node state _ ) evalFunc 0 = evalFunc state
evaluate (Node _ xs) evalFunc depth = Only (-1) * minimum (map (evaluateOutOfOrder evalFunc (depth - 1)) xs) where
        evaluateOutOfOrder = \x y z -> evaluate z x y

generateNegamaxTree :: state -> (move -> state -> state)  -> (state -> Bool) -> (state -> [move]) -> NegamaxTree state
generateNegamaxTree state moveFunc checkGameOverFunc generatePossibleMovesFunc
        | checkGameOverFunc state = Node state []
        | otherwise = Node state listOfNodes
                where listOfNodes = map (argsModifiedGenerateNegamaxTree moveFunc checkGameOverFunc generatePossibleMovesFunc . (flip moveFunc state)) validMoves
                      argsModifiedGenerateNegamaxTree = \a b c d -> generateNegamaxTree d a b c
                      validMoves = generatePossibleMovesFunc state

findBestMove :: forall move state.
        state ->
        (move -> state -> state) ->
        (state -> Bool) ->
        (state -> [move]) ->
        (state -> ExtendedNum Integer) ->
        Int ->
        move
findBestMove state moveFunc checkGameOverFunc generatePossibleMovesFunc evalFunc depth = foldl1 biggerOne moveList where
        biggerOne :: move -> move -> move
        -- Note that we choose the least favorable state because all the
        -- states are calculated from the perspective of the opposing
        -- player
        biggerOne acc newMove = if newMoveScore < accScore
                                   then newMove
                                   else acc
                where newMoveScore = evaluateAB (generateNegamaxTree (moveFunc newMove state) moveFunc checkGameOverFunc generatePossibleMovesFunc) evalFunc depth
                      accScore = evaluateAB (generateNegamaxTree (moveFunc acc state) moveFunc checkGameOverFunc generatePossibleMovesFunc) evalFunc depth
        moveList :: [move]
        moveList = generatePossibleMovesFunc state
