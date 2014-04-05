{-# LANGUAGE FlexibleInstances #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Char
import Test.QuickCheck
import Test.HUnit
import Negamax
import Tictactoe as Tic
import qualified SuperTictactoe as Sup

instance Arbitrary (ExtendedNum Integer) where
        arbitrary = oneof [integerGen, infGen] where
                integerGen = fmap Only (choose (0 :: Integer, 1000000000000000000 :: Integer))
                infGen = elements [PosInf, NegInf]

prop_AbsSignum :: ExtendedNum Integer -> Bool
prop_AbsSignum x = abs x * signum x == x

tests =
        [
        testGroup "Negamax Tests"
                [
                        testProperty "abs and signum satisfies law specified in Num" prop_AbsSignum
                ]
        , testGroup "TicTacToe Tests"
                [
                        testCase "Player 1 finds winning move" test_1,
                        testCase "Player 2 finds winning move" test_2,
                        testCase "Player 2 finds move to block player 1 win" test_3,
                        testCase "Player 1 finds move to block player 2 win" test_4,
                        testCase "isValidMove rejects inputs that imply moves with negative index" test_10,
                        testCase "isValidMove rejects inputs that imply moves with too high of an index" test_11
                ]
        , testGroup "SuperTicTacToe Tests"
                [
                        testCase "Game recognizes Player 1 victory when all top rows are won and are the same mini board" test_5,
                        testCase "Game recognizes Player 1 victory when all top rows are won, but are different mini boards" test_6,
                        testCase "findBestMove AI as player 1 finds winning move" test_7,
                        testCase "findBestMove AI as player 2 finds winning move" test_8,
                        testCase "Game recognizes Player 2 victory when left column is won, but are different mini boards" test_9,
                        testCase "isValidMove rejects inputs that imply moves with negative index" test_12,
                        testCase "isValidMove rejects inputs that imply moves with too high of an index" test_13
                ]
        ]

test_1 = Tic.findBestMove nearlyWinningState1 @?= (2, 0)

test_2 = Tic.findBestMove nearlyWinningState2 @?= (0, 1)

test_3 = Tic.findBestMove nearlyLosingState2 @?= (2, 2)

test_4 = Tic.findBestMove nearlyLosingState1 @?= (2, 1)

test_5 = Sup.checkSuperGameOver winningSuperStateAllMiniBoardsSame @?= Sup.Player1WinSuper

test_6 = Sup.checkSuperGameOver winningSuperStateDifferentMiniBoards @?= Sup.Player1WinSuper

test_7 = Sup.findBestMove nearlyWinningSuperState1 @?= (2, 0)

test_8 = Sup.findBestMove nearlyWinningSuperState2 @?= (0, 1)

test_9 = Sup.checkSuperGameOver winningSuperState2 @?= Sup.Player2WinSuper

test_10 = Tic.isValidMove (-1, 0) Tic.startingBoard || Tic.isValidMove (0, -1) Tic.startingBoard || Tic.isValidMove (-2, -3) Tic.startingBoard @?= False

test_11 = Tic.isValidMove (3, 2) Tic.startingBoard || Tic.isValidMove (3, 2) Tic.startingBoard || Tic.isValidMove (3, 3) Tic.startingBoard @?= False

test_12 = Sup.isValidSuperMove (-1, 0) Sup.startingSuperState || Sup.isValidSuperMove (0, -1 ) Sup.startingSuperState || Sup.isValidSuperMove (-2, -3) Sup.startingSuperState @?= False

test_13 = Sup.isValidSuperMove (3, 2) Sup.startingSuperState || Sup.isValidSuperMove (3, 2) Sup.startingSuperState || Sup.isValidSuperMove (3, 3) Sup.startingSuperState @?= False

nearlyWinningBoard1 :: GameBoard
nearlyWinningBoard1 =
        [
                [Just Player1, Just Player1, Nothing],
                [Just Player2, Just Player2, Nothing],
                [Just Player1, Just Player2, Just Player1]
        ]

nearlyWinningState1 :: GameState
nearlyWinningState1 = PlayState nearlyWinningBoard1 Player1

nearlyWinningBoard2 :: GameBoard
nearlyWinningBoard2 =
        [
                [Just Player2, Nothing, Nothing],
                [Nothing, Just Player1, Nothing],
                [Just Player2, Just Player1, Just Player1]
        ]

nearlyWinningState2 :: GameState
nearlyWinningState2 = PlayState nearlyWinningBoard2 Player2

nearlyLosingBoard2 :: GameBoard
nearlyLosingBoard2 =
        [
                [Just Player1, Nothing, Nothing],
                [Nothing,Just Player1, Nothing],
                [Just Player2, Nothing, Nothing]
        ]

nearlyLosingState2 :: GameState
nearlyLosingState2 = PlayState nearlyLosingBoard2 Player2

winningBoard1 :: GameBoard
winningBoard1 =
        [
                [Just Player1, Just Player1, Just Player1],
                [Just Player2, Just Player2, Just Player1],
                [Just Player1, Just Player2, Just Player1]
        ]

nearlyLosingBoard1 :: GameBoard
nearlyLosingBoard1 =
        [
                [Just Player1, Nothing, Nothing],
                [Just Player2, Just Player2, Nothing],
                [Just Player1, Nothing, Nothing]
        ]

nearlyLosingState1 = PlayState nearlyLosingBoard1 Player1

winningSuperBoardAllMiniBoardsSame :: Sup.SuperGameBoard
winningSuperBoardAllMiniBoardsSame =
        [
                [winningBoard1, winningBoard1, winningBoard1],
                [startingBoard, startingBoard, startingBoard],
                [startingBoard, startingBoard, startingBoard]
        ]

winningSuperStateAllMiniBoardsSame :: Sup.SuperGameState
winningSuperStateAllMiniBoardsSame = Sup.SuperPlayState (2, 2) winningSuperBoardAllMiniBoardsSame Player1

winningBoard2 :: GameBoard
winningBoard2 =
        [
                [Just Player1, Just Player2, Just Player2],
                [Nothing, Just Player1, Nothing],
                [Nothing, Nothing, Just Player1]
        ]

winningSuperBoardDifferentMiniBoards :: Sup.SuperGameBoard
winningSuperBoardDifferentMiniBoards =
        [
                [winningBoard1, winningBoard1, winningBoard2],
                [startingBoard, startingBoard, startingBoard],
                [startingBoard, startingBoard, startingBoard]
        ]

winningSuperStateDifferentMiniBoards :: Sup.SuperGameState
winningSuperStateDifferentMiniBoards = Sup.SuperPlayState (2, 2) winningSuperBoardDifferentMiniBoards Player1

nearlyWinningSuperBoard1 :: Sup.SuperGameBoard
nearlyWinningSuperBoard1 =
        [
                [nearlyWinningBoard1, nearlyWinningBoard1, nearlyWinningBoard1],
                [winningBoard1, winningBoard1, nearlyWinningBoard1],
                [nearlyWinningBoard1, nearlyWinningBoard1, nearlyWinningBoard1]
        ]

nearlyWinningSuperState1 :: Sup.SuperGameState
nearlyWinningSuperState1 = Sup.SuperPlayState (2, 1) nearlyWinningSuperBoard1 Player1

nearlyWinningSuperBoard2 :: Sup.SuperGameBoard
nearlyWinningSuperBoard2 =
        [
                [winningMiniBoard2b, nearlyWinningBoard2, nearlyWinningBoard2],
                [winningMiniBoard2b, nearlyWinningBoard2, nearlyWinningBoard2],
                [nearlyWinningBoard2, nearlyWinningBoard2, winningBoard1]
        ]

nearlyWinningSuperState2 :: Sup.SuperGameState
nearlyWinningSuperState2 = Sup.SuperPlayState (0, 2) nearlyWinningSuperBoard2 Player2

winningMiniBoard2a :: GameBoard
winningMiniBoard2a =
        [
                [Just Player2, Nothing, Nothing],
                [Just Player2, Just Player1, Nothing],
                [Just Player2, Just Player1, Just Player1]
        ]

winningMiniBoard2b :: GameBoard
winningMiniBoard2b =
        [
                [Just Player2, Just Player1, Just Player1],
                [Nothing, Just Player2, Nothing],
                [Nothing, Nothing, Just Player2]
        ]

winningSuperBoard2 :: Sup.SuperGameBoard
winningSuperBoard2 =
        [
                [winningMiniBoard2b, nearlyWinningBoard2, nearlyWinningBoard2],
                [winningMiniBoard2b, nearlyWinningBoard2, nearlyWinningBoard2],
                [winningMiniBoard2a, nearlyWinningBoard2, winningBoard1]
        ]

winningSuperState2 = Sup.SuperPlayState (0, 2) winningSuperBoard2 Player2

fullMiniBoardTie :: GameBoard
fullMiniBoardTie =
        [
                [Just Player1, Just Player1, Just Player2],
                [Just Player2, Just Player2, Just Player1],
                [Just Player1, Just Player1, Just Player2]
        ]

fullMiniBoardTieSuperBoard :: Sup.SuperGameBoard
fullMiniBoardTieSuperBoard =
        [
                [fullMiniBoardTie, startingBoard, startingBoard],
                [startingBoard, startingBoard, startingBoard],
                [startingBoard, startingBoard, startingBoard]
        ]

fullMiniBoardTieState :: Sup.SuperGameState
fullMiniBoardTieState = Sup.SuperPlayState (0, 0) fullMiniBoardTieSuperBoard Player2

main = defaultMain tests
