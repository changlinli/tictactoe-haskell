{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module SuperTictactoe where

import qualified Negamax as Negamax
import qualified Tictactoe as Tic
import qualified Data.List as DL
import qualified Control.Applicative as CA

boardSize = Tic.boardSize
superBoardSize = 3

-- Note that this is hugely inefficient to just have the gameboard. Ideally
-- we would also include which player has won certain subboards of our
-- super board so we don't have to recalculate every time
type SuperGameBoard = [[ Tic.GameBoard ]]
data SuperGameState = SuperPlayState { currentMiniBoard :: (Int, Int), currentSuperBoard :: SuperGameBoard, currentPlayer :: Tic.Players } | Player1WinSuper | Player2WinSuper | TieSuper deriving (Show, Eq)

startingSuperBoard :: SuperGameBoard
startingSuperBoard = [[Tic.startingBoard, Tic.startingBoard, Tic.startingBoard], [Tic.startingBoard, Tic.startingBoard, Tic.startingBoard], [Tic.startingBoard, Tic.startingBoard, Tic.startingBoard]]

startingSuperState :: SuperGameState
startingSuperState = SuperPlayState { currentMiniBoard=(1, 1), currentSuperBoard=startingSuperBoard, currentPlayer=Tic.Player1 }

nextPlayer :: Tic.Players -> Tic.Players
nextPlayer Tic.Player1 = Tic.Player2
nextPlayer Tic.Player2 = Tic.Player1

getGameBoardUnit :: ((Int, Int), (Int, Int)) -> SuperGameBoard -> Tic.GameBoardUnit
getGameBoardUnit ((a, b), (c, d)) board = (((board !! b) !! a) !! d) !! c

evalFunc :: SuperGameState -> Negamax.ExtendedNum Integer
evalFunc = undefined

checkSuperFull :: SuperGameBoard -> Bool
checkSuperFull superBoard = and (map foldRow superBoard)
        where foldRow = foldl (\x y -> Tic.checkFull y && x) True

checkSuperThreeInARow :: [[ Tic.GameBoard ]] -> Maybe Tic.Players
checkSuperThreeInARow listOfRows = True `DL.elemIndex` (map Tic.sameEntries listOfWins) >>= (\n -> head (listOfWins !! n))
        where listOfWins = map (map Tic.findWinner) listOfRows

checkSuperRows :: SuperGameBoard -> Maybe Tic.Players
checkSuperRows superBoard = checkSuperThreeInARow (Tic.getRows superBoard)

checkSuperCols :: SuperGameBoard -> Maybe Tic.Players
checkSuperCols superBoard = checkSuperThreeInARow (Tic.getCols superBoard)

checkSuperDiagonals :: SuperGameBoard -> Maybe Tic.Players
checkSuperDiagonals superBoard = checkSuperThreeInARow (Tic.getDiagonals superBoard)

findSuperWinner :: SuperGameBoard -> Maybe Tic.Players
findSuperWinner superBoard = checkSuperDiagonals superBoard CA.<|> checkSuperRows superBoard CA.<|> checkSuperCols superBoard

checkSuperGameOver :: SuperGameState -> SuperGameState
checkSuperGameOver currentState@(SuperPlayState miniBoard superBoard superPlayer)
        | findSuperWinner superBoard == Just Tic.Player1 = Player1WinSuper
        | findSuperWinner superBoard == Just Tic.Player2 = Player2WinSuper
        | checkSuperFull superBoard = TieSuper
        | otherwise = currentState

isValidSuperMove :: (Int, Int) -> SuperGameState -> Bool
isValidSuperMove (a, b) (SuperPlayState miniBoardCoord superBoard superPlayer) = Tic.isValidMove (a, b) actualBoard
        where actualBoard = getMiniBoard miniBoardCoord superBoard

getMiniBoard :: (Int, Int) -> SuperGameBoard -> Tic.GameBoard
getMiniBoard (a, b) superBoard = (superBoard !! b) !! a

updateSuperState :: SuperGameState -> (Int, Int) -> SuperGameState
updateSuperState SuperPlayState{currentMiniBoard=(a, b), currentSuperBoard=oldSuperBoard, currentPlayer=player} (x, y) =
        SuperPlayState newMiniBoardCoord newBoard newPlayer
        where newBoard =Tic.updateList oldSuperBoard (Tic.updateList (oldSuperBoard !! b) actualBoard a) b
              actualBoard = Tic.updateBoard (getMiniBoard (a, b) oldSuperBoard) (x, y) (Just player)
              newMiniBoardCoord = (x, y)
              newPlayer = Tic.nextPlayer player

playSuperMove :: (Int, Int) -> SuperGameState -> SuperGameState
playSuperMove _ Player1WinSuper = error "The game is already over (Player 1 Won!)"
playSuperMove _ Player2WinSuper = error "The game is already over (Player 2 Won!)"
playSuperMove _ TieSuper = error "The game is already over (there was a tie!)"
playSuperMove (a, b) currentState@(SuperPlayState miniBoardCoord superBoard player)
        | isValidSuperMove (a, b) currentState = updateSuperState currentState (a, b)
        | otherwise = error "Invalid Move!"

(-+-) :: String -> String -> String
-- Laterally combine strings, e.g. "1\na\n" and "2\nb\n" to "12\nab\n"
(-+-) xs ys = unlines (zipWith (++) (lines xs) (lines ys))

showSuperGameBoard :: SuperGameBoard -> String
showSuperGameBoard
        [[a0, a1, a2],
        [b0, b1, b2],
        [c0, c1, c2]] =
        Tic.showGameBoard a0 -+- "{}\n{}\n{}\n" -+- Tic.showGameBoard a1 -+- "{}\n{}\n{}\n" -+- Tic.showGameBoard a2 ++ "\n" ++
        Tic.showGameBoard b0 -+- "{}\n{}\n{}\n" -+- Tic.showGameBoard b1 -+- "{}\n{}\n{}\n" -+- Tic.showGameBoard b2 ++ "\n" ++
        Tic.showGameBoard c0 -+- "{}\n{}\n{}\n" -+- Tic.showGameBoard c1 -+- "{}\n{}\n{}\n" -+- Tic.showGameBoard c2

playGame :: SuperGameState -> IO ()
playGame Player1WinSuper = putStrLn "Player 1 Wins!"
playGame Player2WinSuper = putStrLn "Player 2 Wins!"
playGame TieSuper = putStrLn "There is a Tie!"
playGame currentState@(SuperPlayState {currentMiniBoard=miniBoardCoord, currentSuperBoard=superBoard, currentPlayer=player}) =
        putStrLn (showSuperGameBoard superBoard) >>
        putStrLn ("Current board is " ++ (show miniBoardCoord)) >>=
        (\x -> Tic.getInputWithRetry) >>=
        (\y -> return (playSuperMove y currentState)) >>=
        (\z -> return (checkSuperGameOver z)) >>=
        playGame
