import Data.Sequence as DS
import Data.Foldable as DF

boardSize = 3
data Players = Player1 | Player2 deriving Eq
type GameBoardUnit = Maybe Players
type GameBoard = Seq (Seq GameBoardUnit)
data GameState = PlayState { board :: GameBoard, currentPlayer :: Players } | Player1Win | Player2Win | Tie

nextPlayer :: Players -> Players
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

isValidMove :: (Int, Int) -> GameBoard -> Bool
isValidMove (a, b) board = if index (index board b) a == Nothing && a < boardSize && b < boardSize
           then True
           else False

startingBoard :: GameBoard
startingBoard = DS.fromList [DS.fromList [Nothing, Nothing, Nothing], DS.fromList [Nothing, Nothing, Nothing], DS.fromList [Nothing, Nothing, Nothing]]

startingState :: GameState
startingState = PlayState { board=startingBoard, currentPlayer=Player1 }

-- TODO: Use state monad instead of self-hacked solution
playMove :: (Int, Int) -> GameState -> GameState
playMove _ Player1Win = error "The game is already over (Player 1 Won!)"
playMove _ Player2Win = error "The game is already over (Player 2 Won!)"
playMove _ Tie = error "The game is already over (there was a tie!)"
playMove (a, b) (PlayState board player)
        | isValidMove (a, b) board = PlayState { board=newBoard, currentPlayer=nextPlayer player }
        | otherwise = error "Invalid Move!"
        where 
                newBoard = DS.update b (DS.update a (Just player) (index board b)) board

blah = DS.fromList [DS.fromList [1, 2, 3], 
                    DS.fromList [1, 2, 3], 
                    DS.fromList [1, 2, 3]]

-- TODO: Make diagonals more general
diagonals :: GameBoard -> [[GameBoardUnit]]
diagonals board = 
        [
                [index (index board 1) 1, index (index board 2) 2, index (index board 3) 3],
                [index (index board 1) 3, index (index board 2) 2, index (index board 3) 1]
        ]

rows :: GameBoard -> [[GameBoardUnit]]
rows board = DF.toList $ fmap DF.toList board

{-transposeBoard :: GameBoard -> GameBoard-}
{-transposeBoard board = DS.fromList [x | x <- DF.toList -}

{-checkGameOver :: GameState -> GameState-}
{-checkGameOver PlayState board _ = any-}
