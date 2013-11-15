data NegamaxTree a = EmptyTree | Node a [NegamaxTree a]

singleton :: a -> NegamaxTree a
singleton x = Node x [ EmptyTree ]

{-evaluate :: (Ord a) => NegamaxTree a -> a-}
{-evaluate (Node x ys) = foldl (\x (Node a bs) = max x a-}
