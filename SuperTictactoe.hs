{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SuperTictactoe where

import qualified Negamax as Negamax
import qualified Tictactoe as Tic
import qualified Data.List as DL
import qualified Control.Applicative as CA

boardSize :: Int
boardSize = Tic.boardSize

superBoardSize :: Int
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
evalFunc state@SuperPlayState{currentSuperBoard=superBoard, currentPlayer=player}
        | player == Tic.Player1 && checkSuperGameOver state == Player1WinSuper = Negamax.PosInf
        | player == Tic.Player2 && checkSuperGameOver state == Player2WinSuper = Negamax.PosInf
        | player == Tic.Player1 && checkSuperGameOver state == Player2WinSuper = Negamax.NegInf
        | player == Tic.Player2 && checkSuperGameOver state == Player1WinSuper = Negamax.NegInf
        | checkSuperGameOver state == TieSuper = Negamax.Only 0
        | otherwise = foldl ( \x y -> x + winnerScore y) 0 (fmap (flip getMiniBoard superBoard) Tic.allPossiblePairs) where
        winnerScore miniBoard = case Tic.findWinner miniBoard of
                x | x == Just player -> 1
                x | x == Just (Tic.nextPlayer player) -> (-1)
                _ -> 0
evalFunc _ = error "Can only use evaluation function on a game state with a board"

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
checkSuperGameOver currentState
        | findSuperWinner superBoard == Just Tic.Player1 = Player1WinSuper
        | findSuperWinner superBoard == Just Tic.Player2 = Player2WinSuper
        | checkSuperFull superBoard = TieSuper
        | otherwise = currentState
        where superBoard = currentSuperBoard currentState

isSuperGameOver :: SuperGameState -> Bool
isSuperGameOver state = if checkSuperGameOver state /= state
                           then True
                           else False

isValidSuperMove :: (Int, Int) -> SuperGameState -> Bool
isValidSuperMove (a, b) SuperPlayState{currentMiniBoard=miniBoardCoord, currentSuperBoard=superBoard} = Tic.isValidMove (a, b) actualBoard && sizeCond
        where actualBoard = getMiniBoard miniBoardCoord superBoard
              sizeCond = a < superBoardSize && b < superBoardSize
isValidSuperMove _ _ = error "Cannot determine the validity of a move when there is no board in the game state!"

getMiniBoard :: (Int, Int) -> SuperGameBoard -> Tic.GameBoard
getMiniBoard (a, b) superBoard = (superBoard !! b) !! a

updateSuperState :: SuperGameState -> (Int, Int) -> SuperGameState
updateSuperState SuperPlayState{currentMiniBoard=(a, b), currentSuperBoard=oldSuperBoard, currentPlayer=player} (x, y) =
        SuperPlayState newMiniBoardCoord newBoard newPlayer
        where newBoard =Tic.updateList oldSuperBoard (Tic.updateList (oldSuperBoard !! b) actualBoard a) b
              actualBoard = Tic.updateBoard (getMiniBoard (a, b) oldSuperBoard) (x, y) (Just player)
              newMiniBoardCoord = (x, y)
              newPlayer = Tic.nextPlayer player
updateSuperState x _ = x

playSuperMove :: (Int, Int) -> SuperGameState -> SuperGameState
playSuperMove _ Player1WinSuper = error "The game is already over (Player 1 Won!)"
playSuperMove _ Player2WinSuper = error "The game is already over (Player 2 Won!)"
playSuperMove _ TieSuper = error "The game is already over (there was a tie!)"
playSuperMove (a, b) currentState = updateSuperState currentState (a, b)

(-+-) :: String -> String -> String
-- Laterally combine strings, e.g. "1\na\n" and "2\nb\n" to "12\nab\n"
(-+-) xs ys = unlines (zipWith (++) (lines xs) (lines ys))

showSuperGameBoard :: SuperGameBoard -> String
showSuperGameBoard superBoard = Tic.showBoard superBoard "{}\n{}\n{}\n" Tic.showGameBoard (-+-)

maximumDepth :: Int
maximumDepth = 4

allPossiblePairs :: [(Int, Int)]
allPossiblePairs = Tic.enumPair (0, 0) (superBoardSize - 1, superBoardSize - 1)

generateValidSuperMoves :: SuperGameState -> [(Int, Int)]
generateValidSuperMoves superState = filter (flip Tic.isValidMove (getMiniBoard miniBoardCoord superBoard)) allPossiblePairs
        where miniBoardCoord = currentMiniBoard superState
              superBoard = currentSuperBoard superState

findBestMove :: SuperGameState -> (Int, Int)
findBestMove state = Negamax.findBestMove state playSuperMove isSuperGameOver generateValidSuperMoves evalFunc maximumDepth

playSuperMoveWithRetry :: (Int, Int) -> SuperGameState -> IO SuperGameState
playSuperMoveWithRetry moveCoord currentState =
        Tic.retry
                moveCoord
                (\x -> isValidSuperMove x currentState)
                (playSuperMoveIO currentState)
                (putStrLn "Invalid move!" >> Tic.getInputWithRetry)
        where playSuperMoveIO = \x y -> return $ playSuperMove y x

playAIMove :: SuperGameState -> IO SuperGameState
playAIMove state = do
        putStrLn (showSuperGameBoard superBoard)
        putStrLn ("Current board is " ++ (show miniBoardCoord))
        putStrLn "AI is playing now"
        x <- return (findBestMove state)
        y <- return (playSuperMove x state)
        return (checkSuperGameOver y)
        where
                superBoard = currentSuperBoard state
                miniBoardCoord = currentMiniBoard state

playHumanMove :: SuperGameState -> IO SuperGameState
playHumanMove state = do
        putStrLn (showSuperGameBoard superBoard)
        putStrLn ("Current board is " ++ (show miniBoardCoord))
        x <- Tic.getInputWithRetry
        y <- playSuperMoveWithRetry x state
        return (checkSuperGameOver y)
        where
                superBoard = currentSuperBoard state
                miniBoardCoord = currentMiniBoard state

playGameAI :: Int -> SuperGameState -> IO ()
playGameAI _ Player1WinSuper = putStrLn "Player 1 Wins!"
playGameAI _ Player2WinSuper = putStrLn "Player 2 Wins!"
playGameAI _ TieSuper = putStrLn "There is a tie!"
playGameAI 0 state = playHumanMove state >>= playGameAI 0

playGameAI 1 state
        | player == Tic.Player1 = playHumanMove state >>= playGameAI 1
        | player == Tic.Player2 = playAIMove state >>= playGameAI 1
        where player = currentPlayer state

playGameAI 2 state
        | player == Tic.Player2 = playHumanMove state >>= playGameAI 2
        | player == Tic.Player1 = playAIMove state >>= playGameAI 2
        where player = currentPlayer state

playGameAI 3 state = playAIMove state >>= playGameAI 3
playGameAI _ _ = error "Can only use 0, 1, 2, 3 in the first argument of playGameAI!"
