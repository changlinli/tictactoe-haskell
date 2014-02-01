{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

boardSize = 3
superBoardSize = 3
data Players = Player1 | Player2 deriving (Eq, Show)
type GameBoardUnit = Maybe Players

-- The use of all the pragmas was to justify this syntactic sugar of the
-- use of show. If we don't want the pragmas, just make a new function
-- instead of using show to display the game board
instance Show GameBoardUnit where
        show (Just Player1) = "X"
        show (Just Player2) = "O"
        show Nothing = " "

type GameBoard = [[GameBoardUnit]]
data GameState = PlayState { board :: GameBoard, currentPlayer :: Players } | Player1Win | Player2Win | Tie deriving (Show, Eq)

type SuperGameBoard = [[ GameBoard ]]
data SuperGameState = SuperPlayState { currentSuperBoard :: SuperGameBoard, currentSuperPlayer :: Players } | Player1WinSuper | Player2WinSuper | TieSuper deriving (Show, Eq)

nextPlayer :: Players -> Players
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

getGameBoardUnit :: ((Int, Int), (Int, Int)) -> SuperGameBoard -> GameBoardUnit
getGameBoardUnit ((a, b), (c, d)) board = (((board !! b) !! a) !! d) !! c

isValidMove :: ((Int, Int), (Int, Int)) -> SuperGameBoard -> Bool
isValidMove coordinates@((a, b), (c, d)) board =
        if getGameBoardUnit coordinates board  == Nothing && c < boardSize && d < boardSize && a < superBoardSize && b < superBoardSize
           then True
           else False
