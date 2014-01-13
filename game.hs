import qualified Data.Sequence as DS
import qualified Data.Foldable as DF

boardSize = 3
data Players = Player1 | Player2 deriving Eq
type GameBoardUnit = Maybe Players
type GameBoard = DS.Seq (DS.Seq GameBoardUnit)
data GameState = PlayState { board :: GameBoard, currentPlayer :: Players } | Player1Win | Player2Win | Tie

nextPlayer :: Players -> Players
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

isValidMove :: (Int, Int) -> GameBoard -> Bool
isValidMove (a, b) board = if DS.index (DS.index board b) a == Nothing && a < boardSize && b < boardSize
           then True
           else False

startingBoard :: GameBoard
startingBoard = DS.fromList [DS.fromList [Nothing, Nothing, Nothing], DS.fromList [Nothing, Nothing, Nothing], DS.fromList [Nothing, Nothing, Nothing]]
-- startingBoard = [[Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing]]

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
                newBoard = DS.update b (DS.update a (Just player) (DS.index board b)) board

blah = DS.fromList [DS.fromList [1, 2, 3], 
                    DS.fromList [1, 2, 3], 
                    DS.fromList [1, 2, 3]]

-- TODO: Make diagonals more general
diagonals :: GameBoard -> [[GameBoardUnit]]
diagonals board = 
        [
                [DS.index (DS.index board 1) 1, DS.index (DS.index board 2) 2, DS.index (DS.index board 3) 3],
                [DS.index (DS.index board 1) 3, DS.index (DS.index board 2) 2, DS.index (DS.index board 3) 1]
        ]

rows :: GameBoard -> [[GameBoardUnit]]
rows board = DF.toList $ fmap DF.toList board

{-transposeBoard :: GameBoard -> GameBoard-}
{-transposeBoard board = DS.fromList [x | x <- DF.toList -}

{-checkGameOver :: GameState -> GameState-}
{-checkGameOver PlayState board _ = any-}

checkDiagonal :: GameBoard -> Maybe Players
checkDiagonal board 
        | [Just Player1, Just Player1, Just Player1] `elem` diagonals board = Just Player1
        | [Just Player2, Just Player2, Just Player2] `elem` diagonals board = Just Player2
        | otherwise = Nothing
