data ExtendedNum a = Only a | NegInf | PosInf
data NegamaxTree a state = EmptyTree | Node a state [NegamaxTree a state]

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
