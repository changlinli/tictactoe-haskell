data ExtendedNum a = Only a | NegInf | PosInf
data NegamaxTree a state = EmptyTree | Node a state [NegamaxTree a state]

singleton :: a -> state -> NegamaxTree a state
singleton x state = Node x state [ EmptyTree ]

extendedNum2Num :: (Num a) => ExtendedNum a -> a
extendedNum2Num NegInf = error "Things went wrong!"
extendedNum2Num PosInf = error "Things went wrong!"
extendedNum2Num (Only x) = x

evaluate :: ((NegamaxTree (ExtendedNum Integer) state) -> ExtendedNum Integer) -> (NegamaxTree (ExtendedNum Integer) state) -> (ExtendedNum Integer)
evaluate f EmptyTree = NegInf
{-evaluate f (Node x state) = foldl (\x (Node a bs) -> max x a) x ys-}
