{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

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
data GameState = PlayState { board :: GameBoard, currentPlayer :: Players } | Player1Win | Player2Win | Tie deriving Show

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
getDiagonals :: GameBoard -> [[GameBoardUnit]]
getDiagonals
        [[ x1, _ , x3],
        [_, y1 , _],
        [z1, _ , z3]] = [[x1, y1, z3], [x3, y1, z1]]

-- Yes I know, this does nothing, but it might if GameBoard changes from
-- just being a list of lists
getRows :: GameBoard -> [[GameBoardUnit]]
getRows board = board

getCols :: GameBoard -> [[GameBoardUnit]]
getCols board = transposeBoard board

transposeBoard :: GameBoard -> GameBoard
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

counter :: Int -> IO ()
counter x = print x >>= (\y -> counter (x + 1))

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

evalFunc1 :: GameState -> Negamax.ExtendedNum Integer
-- WHOA! Haskell will automatically promote 0 to Only 0!
evalFunc1 Tie = 0
evalFunc1 Player1Win = Negamax.PosInf
evalFunc1 Player2Win = Negamax.NegInf
evalFunc1 x = 0

evalFunc2 :: GameState -> Negamax.ExtendedNum Integer
evalFunc2 Tie = 0
evalFunc2 Player1Win = Negamax.NegInf
evalFunc2 Player2Win = Negamax.PosInf
evalFunc2 x = 0

enumPair :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
enumPair (a0, b0) (a1, b1) = [a0 .. a1] >>= \x -> [b0 .. b1] >>= \y -> return (x, y)

allPossiblePairs :: [(Int, Int)]
allPossiblePairs = enumPair (0, 0) (3, 3)

generateValidMoves :: GameState -> [(Int, Int)] -> [(Int, Int)]
generateValidMoves (PlayState board player) xs = filter (flip isValidMove board) xs

generateNegamaxTree :: GameState -> Negamax.NegamaxTree GameState
generateNegamaxTree Player1Win = Negamax.Node Player1Win []
generateNegamaxTree Player2Win = Negamax.Node Player2Win []
generateNegamaxTree Tie = Negamax.Node Tie []
generateNegamaxTree state = Negamax.Node state listOfNodes
        where listOfNodes = map (generateNegamaxTree . (flip playMove state)) validMoves
              validMoves = generateValidMoves state allPossiblePairs

generateListOfTreesAndMoves :: GameState -> [Negamax.NegamaxTree GameState]
generateListOfTreesAndMoves state = map generateNegamaxTree (map (flip playMove state) validMoves) where
        validMoves = generateValidMoves state allPossiblePairs


playGameAI :: GameState -> IO ()
playGameAI Player1Win = putStrLn "Player 1 wins!"
playGameAI Player2Win = putStrLn "Player 2 wins!"
playGameAI Tie = putStrLn "There is a tie!"
playGameAI state@(PlayState {board=board, currentPlayer=player})
        | player == Player1 = putStrLn (showGameBoard board) >> getInput >>= (\x -> return ((flip playMove) PlayState {board=board, currentPlayer=player} x)) >>= (\y -> return (checkGameOver y)) >>= playGameAI
        {-| player == Player2 = putStrLn (showGameBoard board) >>-}

main :: IO ()
main = playGame startingState
