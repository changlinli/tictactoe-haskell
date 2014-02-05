{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

import qualified Data.Sequence as DS
import qualified Data.Foldable as DF
import qualified Data.List as DL
import qualified Negamax
import qualified Tictactoe as Tic

main :: IO ()
main = do
        putStrLn "Please enter 0 for human vs. human, 1 for human (player 1) vs. AI (player 2), 2 for AI (player 1) vs human (player 2), and 3 for AI vs AI"
        stringGameType <- getLine
        let gameType = read stringGameType :: Int
        (putStrLn . Tic.showGameBoard) (Tic.board Tic.startingState)
        (Tic.selectGameType gameType) Tic.startingState
