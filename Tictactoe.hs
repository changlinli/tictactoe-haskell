{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Tictactoe where

import qualified Data.Sequence as DS
import qualified Data.Foldable as DF
import qualified Data.List as DL
import qualified Negamax

boardSize = 3
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

nextPlayer :: Players -> Players
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

isValidMove :: (Int, Int) -> GameBoard -> Bool
isValidMove (a, b) board = if (board !! b) !! a == Nothing && a < boardSize && b < boardSize
           then True
           else False

startingBoard :: GameBoard
startingBoard = [[Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing]]

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
                newBoard = updateBoard board (a, b) (Just player)

-- TODO: Make diagonals more general
getDiagonals :: [[a]] -> [[a]]
getDiagonals
        [[ x1, _ , x3],
        [_, y1 , _],
        [z1, _ , z3]] = [[x1, y1, z3], [x3, y1, z1]]

-- Yes I know, this does nothing, but it might if GameBoard changes from
-- just being a list of lists
getRows :: [[a]] -> [[a]]
getRows board = board

getCols :: [[a]] -> [[a]]
getCols board = transposeBoard board

transposeBoard :: [[a]] -> [[a]]
transposeBoard board = DL.transpose board

checkRows :: GameBoard -> Maybe Players
checkRows board = checkThreeInARow (getRows board)

checkCols :: GameBoard -> Maybe Players
checkCols board = checkThreeInARow (getCols board)

checkGameOver :: GameState -> GameState
checkGameOver (PlayState board player)
        | checkDiagonals board == Just Player1 = Player1Win
        | checkDiagonals board == Just Player2 = Player2Win
        | checkRows board == Just Player1 = Player1Win
        | checkRows board == Just Player2 = Player2Win
        | checkCols board == Just Player1 = Player1Win
        | checkCols board == Just Player2 = Player2Win
        | checkFull board = Tie
        | otherwise = PlayState {board=board, currentPlayer=player}

checkFull :: GameBoard -> Bool
checkFull [row]
        | Nothing `elem` row = False
        | otherwise = True
checkFull (x:xs)
        | not (checkFull [x]) = checkFull [x]
        | otherwise = checkFull xs

updateList :: [a] -> a -> Int -> [a]
updateList (x:xs) newItem index
        | index == 0 = newItem:xs
        | otherwise = x:(updateList xs newItem (index - 1))

updateBoard :: GameBoard -> (Int, Int) -> GameBoardUnit -> GameBoard
updateBoard oldBoard (x, y) value = updateList oldBoard (updateList (oldBoard !! y) value x) y

checkThreeInARow :: [[GameBoardUnit]] -> Maybe Players
checkThreeInARow listOfRows
        | [Just Player1, Just Player1, Just Player1] `elem` listOfRows = Just Player1
        | [Just Player2, Just Player2, Just Player2] `elem` listOfRows = Just Player2
        | otherwise = Nothing

checkDiagonals :: GameBoard -> Maybe Players
checkDiagonals board = checkThreeInARow (getDiagonals board)

showGameBoard :: GameBoard -> String
showGameBoard
        [[a0, a1, a2],
        [b0, b1, b2],
        [c0, c1, c2]] =
        show a0 ++ "|" ++ show a1 ++ "|" ++ show a2 ++ "\n" ++
        show b0 ++ "|" ++ show b1 ++ "|" ++ show b2 ++ "\n" ++
        show c0 ++ "|" ++ show c1 ++ "|" ++ show c2 ++ "\n"

evalFunc :: GameState -> Negamax.ExtendedNum Integer
evalFunc state@(PlayState {board=board, currentPlayer=player})
        | player == Player1 && checkGameOver state == Player1Win = Negamax.PosInf
        | player == Player2 && checkGameOver state == Player2Win = Negamax.PosInf
        | player == Player1 && checkGameOver state == Player2Win = Negamax.NegInf
        | player == Player2 && checkGameOver state == Player1Win = Negamax.NegInf
        | checkGameOver state == Tie = Negamax.Only 0
        | otherwise = Negamax.Only 0

enumPair :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
enumPair (a0, b0) (a1, b1) = [a0 .. a1] >>= \x -> [b0 .. b1] >>= \y -> return (x, y)

allPossiblePairs :: [(Int, Int)]
allPossiblePairs = enumPair (0, 0) (boardSize - 1, boardSize - 1)

generateValidMoves :: GameState -> [(Int, Int)] -> [(Int, Int)]
generateValidMoves (PlayState board player) xs = filter (flip isValidMove board) xs

generateNegamaxTree :: GameState -> Negamax.NegamaxTree GameState
generateNegamaxTree state@(PlayState board player)
        | checkGameOver state /= state = Negamax.Node state []
        | otherwise = Negamax.Node (checkGameOver state) listOfNodes
                where listOfNodes = map (generateNegamaxTree . (flip playMove state)) validMoves
                      validMoves = generateValidMoves state allPossiblePairs

generateListOfTreesAndMoves :: GameState -> [(Negamax.NegamaxTree GameState, (Int, Int))]
generateListOfTreesAndMoves state = map (\(x, y) -> (generateNegamaxTree x, y)) (map (\(a, b) -> (playMove a state, b)) validMovesTwoCopies) where
        validMovesTwoCopies = zip validMoves validMoves
        validMoves = generateValidMoves state allPossiblePairs

evaluateState :: GameState -> Negamax.ExtendedNum Integer
evaluateState state = Negamax.evaluate (generateNegamaxTree state) evalFunc 10

findBestMove :: GameState -> (Int, Int)
findBestMove state = foldl1 biggerOne moveList where
        biggerOne :: (Int, Int) -> (Int, Int) -> (Int, Int)
        -- Note that we choose the least favorable state because all the
        -- states are calculated from the perspective of the opposing
        -- player
        biggerOne acc newMove = if evaluateState (playMove newMove state) < evaluateState (playMove acc state)
                                   then newMove
                                   else acc
        moveList = generateValidMoves state allPossiblePairs

-- Everything below is tainted by IO!

playGame :: GameState -> IO ()
playGame Player1Win = putStrLn "Player 1 wins!"
playGame Player2Win = putStrLn "Player 2 wins!"
playGame Tie = putStrLn "There is a tie!"
{-playGame PlayState {board=board, currentPlayer=player} = putStrLn (showGameBoard board) >> getInput >>= (\x -> return ((flip playMove) PlayState {board=board, currentPlayer=player} x)) >>= (\y -> return (checkGameOver y)) >>= playGame-}
playGame PlayState {board=board, currentPlayer=player} = do
        putStrLn (showGameBoard board)
        x <- getInput
        y <- return ((flip playMove) PlayState {board=board, currentPlayer=player} x)
        z <- return (checkGameOver y)
        playGame z

getInput :: IO (Int, Int)
getInput = putStrLn "Enter a move!" >>= (\x -> fmap read getLine)

showMoveResult :: (Int, Int) -> GameState -> IO GameState
showMoveResult move state@(PlayState {board=board, currentPlayer=player}) =
        return ((flip playMove) state move) >>=
        (\y@PlayState {board=changed, currentPlayer=newPlayer} -> putStrLn (showGameBoard changed) >> return (checkGameOver y))

playGameAI :: Int -> GameState -> IO ()
playGameAI _ Player1Win = putStrLn "Player 1 wins!"
playGameAI _ Player2Win = putStrLn "Player 2 wins!"
playGameAI _ Tie = putStrLn "There is a tie!"
playGameAI 1 state@(PlayState {board=board, currentPlayer=player})
        | player == Player1 = getInput >>= (\x -> showMoveResult x state) >>= playGameAI 1
        | player == Player2 = return (findBestMove state) >>= (\x -> showMoveResult x state) >>= playGameAI 1
playGameAI 2 state@(PlayState {board=board, currentPlayer=player})
        | player == Player2 = getInput >>= (\x -> showMoveResult x state) >>= playGameAI 2
        | player == Player1 = return (findBestMove state) >>= (\x -> showMoveResult x state) >>= playGameAI 2
playGameAI 3 state@(PlayState {board=board, currentPlayer=player}) =
        return (findBestMove state) >>= (\x -> showMoveResult x state) >>= playGameAI 3

selectGameType :: Int -> (GameState -> IO ())
selectGameType 0 = playGame
selectGameType 1 = playGameAI 1
selectGameType 2 = playGameAI 2
selectGameType 3 = playGameAI 3
