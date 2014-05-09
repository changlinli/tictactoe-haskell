{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Tictactoe where

import qualified Data.List as DL
import qualified Negamax
import qualified Control.Applicative as CA
import qualified Data.Maybe as DM

boardSize :: Int
boardSize = 3

data Players = Player1 | Player2 deriving (Eq, Show)

type GameBoardUnit = Maybe Players

showGameBoardUnit :: GameBoardUnit -> String
showGameBoardUnit (Just Player1) = "X"
showGameBoardUnit (Just Player2) = "O"
showGameBoardUnit Nothing = " "

type GameBoard = [[GameBoardUnit]]
data GameState = PlayState { currentBoard :: GameBoard, currentPlayer :: Players } | Player1Win | Player2Win | Tie deriving (Show, Eq)

nextPlayer :: Players -> Players
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

isValidMove :: (Int, Int) -> GameBoard -> Bool
isValidMove (a, b) board = if a < boardSize &&
                              b < boardSize &&
                              0 <= a &&
                              0 <= b &&
                              (board !! b) !! a == Nothing
           then True
           else False

startingBoard :: GameBoard
startingBoard = [[Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing]]

startingState :: GameState
startingState = PlayState { currentBoard=startingBoard, currentPlayer=Player1 }

-- TODO: Use state monad instead of self-hacked solution
playMove :: (Int, Int) -> GameState -> GameState
playMove _ Player1Win = error "The game is already over (Player 1 Won!)"
playMove _ Player2Win = error "The game is already over (Player 2 Won!)"
playMove _ Tie = error "The game is already over (there was a tie!)"
playMove (a, b) (PlayState board player)= PlayState { currentBoard=newBoard, currentPlayer=nextPlayer player }
        where
                newBoard = updateBoard board (a, b) (Just player)

-- TODO: Make diagonals more general
getDiagonals :: [[a]] -> [[a]]
getDiagonals
        [[ x1, _ , x3],
        [_, y1 , _],
        [z1, _ , z3]] = [[x1, y1, z3], [x3, y1, z1]]
getDiagonals ys@(x:_) = [getLRDiagonals ys 0, getRLDiagonals ys (length x)]
getDiagonals [] = [[],[]]

-- Left to right diagonals
getLRDiagonals :: [[a]] -> Int -> [a]
getLRDiagonals (x:xs) start = (x !! start) : (getLRDiagonals xs (start + 1))
getLRDiagonals [] _ = []

-- Right to left diagonals
getRLDiagonals :: [[a]] -> Int -> [a]
getRLDiagonals (x:xs) start = (x !! start) : (getRLDiagonals xs (start - 1))
getRLDiagonals [] _ = []

-- Yes I know, this does nothing, but it might if GameBoard changes from
-- just being a list of lists
getRows :: [[a]] -> [[a]]
getRows board = board

getCols :: [[a]] -> [[a]]
-- For performance reasons, I have a specific 3x3 case which is faster than
-- transposing
getCols
        [[ x1, x2, x3],
        [y1, y2, y3],
        [z1, z2, z3]] = [[x1, y1, z1], [x2, y2, z2], [x3, y3, z3]]
getCols board = transposeBoard board

transposeBoard :: [[a]] -> [[a]]
transposeBoard board = DL.transpose board

checkRows :: GameBoard -> Maybe Players
checkRows board = checkThreeInARow (getRows board)

checkCols :: GameBoard -> Maybe Players
checkCols board = checkThreeInARow (getCols board)

findWinner :: GameBoard -> Maybe Players
findWinner board = checkDiagonals board CA.<|> checkRows board CA.<|> checkCols board

checkGameOver :: GameState -> GameState
checkGameOver (PlayState board player)
        | findWinner board == Just Player1 = Player1Win
        | findWinner board == Just Player2 = Player2Win
        | checkFull board = Tie
        | otherwise = PlayState {currentBoard=board, currentPlayer=player}
checkGameOver x = x

isGameOver :: GameState -> Bool
isGameOver state = if checkGameOver state /= state
                      then True
                      else False

checkFull :: GameBoard -> Bool
checkFull [] = True -- For totality's sake
checkFull [row]
        | Nothing `elem` row = False
        | otherwise = True
checkFull (x:xs)
        | not (checkFull [x]) = checkFull [x]
        | otherwise = checkFull xs

updateList :: [a] -> a -> Int -> [a]
updateList [] _ _ = error "updateList: Tried to update an empty list!"
updateList (x:xs) newItem index
        | index == 0 = newItem:xs
        | otherwise = x:(updateList xs newItem (index - 1))

updateBoard :: GameBoard -> (Int, Int) -> GameBoardUnit -> GameBoard
updateBoard oldBoard (x, y) value = updateList oldBoard (updateList (oldBoard !! y) value x) y

checkThreeInARow :: (Eq a) => [[ Maybe a ]] -> Maybe a
checkThreeInARow listOfRows = True `DL.elemIndex` (map sameEntries listOfRows) >>= (\n -> head (listOfRows !! n))

sameEntries :: (Eq a) => [a] -> Bool
sameEntries [] = True
sameEntries xs = and $ map (== head xs) (tail xs)

checkDiagonals :: GameBoard -> Maybe Players
checkDiagonals board = checkThreeInARow (getDiagonals board)

showBoard ::
        [[a]] ->
        String ->
        (a -> String) ->
        (String -> String -> String) ->
        String
showBoard board interleaveStr showUnit concatFunc = foldl showRow "" board where
        showRow acc (x:xs) = acc ++ (foldl accRow (showUnit x) xs) ++ "\n"
        showRow acc [] = acc ++ "\n"
        accRow subAcc rowUnit = subAcc `concatFunc` interleaveStr `concatFunc` showUnit rowUnit


showGameBoard :: GameBoard -> String
showGameBoard board = showBoard board "|" showGameBoardUnit (++)

evalFunc :: GameState -> Negamax.ExtendedNum Integer
evalFunc state@(PlayState {currentPlayer=player})
        | player == Player1 && checkGameOver state == Player1Win = Negamax.PosInf
        | player == Player2 && checkGameOver state == Player2Win = Negamax.PosInf
        | player == Player1 && checkGameOver state == Player2Win = Negamax.NegInf
        | player == Player2 && checkGameOver state == Player1Win = Negamax.NegInf
        | checkGameOver state == Tie = Negamax.Only 0
        | otherwise = Negamax.Only 0
evalFunc _ = error "evalFunc: Called evalFunc on a state which has no board to evaluate!"

enumPair :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
enumPair (a0, b0) (a1, b1) = [a0 .. a1] >>= \x -> [b0 .. b1] >>= \y -> return (x, y)

allPossiblePairs :: [(Int, Int)]
allPossiblePairs = enumPair (0, 0) (boardSize - 1, boardSize - 1)

generateValidMoves :: GameState -> [(Int, Int)] -> [(Int, Int)]
generateValidMoves (PlayState board _) xs = filter (flip isValidMove board) xs
generateValidMoves _ _ = []

maximumDepth :: Int
maximumDepth = 10

findBestMove :: GameState -> (Int, Int)
findBestMove state = Negamax.findBestMove state playMove isGameOver (flip generateValidMoves allPossiblePairs) evalFunc maximumDepth

-- Everything below is tainted by IO!

retry :: a -> (a -> Bool) -> (a -> IO b) -> IO a -> IO b
retry input condition moveAction redoAction = if condition input
                                           then moveAction input
                                           else redoAction >>= (\a -> retry a condition moveAction redoAction)

playMoveRobust :: (Int, Int) -> GameState -> IO GameState
playMoveRobust moveCoord currentState@(PlayState board _) =
        retry
                moveCoord
                (\x -> isValidMove x board)
                (playMoveIO currentState)
                (putStrLn "Invalid move!" >> getInputWithRetry)
        where playMoveIO = \x y -> return $ playMove y x
playMoveRobust _ x = putStrLn "This move didn't do anything because the game is already over!" >> return x

maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . DM.listToMaybe . reads

getInput :: IO (Maybe (Int, Int))
getInput = putStrLn "Enter a move! Enter in tuple format, e.g. (1, 2)." >>= (\_ -> fmap maybeRead getLine)

getInputWithRetry :: IO (Int, Int)
getInputWithRetry = getInput >>= (\x -> if DM.isNothing x
                       then putStrLn "Error in parsing input, try again" >> getInputWithRetry
                       else return (DM.fromJust x))

showMoveResult :: (Int, Int) -> GameState -> IO GameState
showMoveResult move state =
        playMoveRobust move state >>=
        (\y@PlayState {currentBoard=changed} -> putStrLn (showGameBoard changed) >> return (checkGameOver y))

playGameAI :: Int -> GameState -> IO ()
playGameAI _ Player1Win = putStrLn "Player 1 wins!"
playGameAI _ Player2Win = putStrLn "Player 2 wins!"
playGameAI _ Tie = putStrLn "There is a tie!"
playGameAI 0 PlayState {currentBoard=board, currentPlayer=player} = do
        putStrLn (showGameBoard board)
        x <- getInputWithRetry
        y <- playMoveRobust x (PlayState {currentBoard=board, currentPlayer=player})
        z <- return (checkGameOver y)
        playGameAI 0 z
playGameAI 1 state
        | currentPlayer state == Player1 = getInputWithRetry >>= (\x -> showMoveResult x state) >>= playGameAI 1
        | currentPlayer state == Player2 = return (findBestMove state) >>= (\x -> showMoveResult x state) >>= playGameAI 1
playGameAI 2 state
        | currentPlayer state == Player2 = getInputWithRetry >>= (\x -> showMoveResult x state) >>= playGameAI 2
        | currentPlayer state == Player1 = return (findBestMove state) >>= (\x -> showMoveResult x state) >>= playGameAI 2
playGameAI 3 state =
        return (findBestMove state) >>= (\x -> showMoveResult x state) >>= playGameAI 3
playGameAI _ _ = error "Can only use 0, 1, 2, 3 in the first argument of playGameAI!"
