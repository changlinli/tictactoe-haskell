{-# LANGUAGE FlexibleInstances #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Char
import Test.QuickCheck
import Test.HUnit
import Negamax
import Tictactoe
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
                        testCase "Player 1 finds move to block player 2 win" test_4
                ]
        , testGroup "SuperTicTacToe Tests"
                [
                        testCase "Game recognizes Player 1 victory when all top rows are won and are the same mini board" test_5,
                        testCase "Game recognizes Player 1 victory when all top rows are won, but are different mini boards" test_6
                ]
        ]

test_1 = findBestMove nearlyWinningState1 @?= (2, 0)

test_2 = findBestMove nearlyWinningState2 @?= (0, 1)

test_3 = findBestMove nearlyLosingState2 @?= (2, 2)

test_4 = findBestMove nearlyLosingState1 @?= (2, 1)

test_5 = Sup.checkSuperGameOver winningSuperStateAllMiniBoardsSame @?= Sup.Player1WinSuper

test_6 = Sup.checkSuperGameOver winningSuperStateDifferentMiniBoards @?= Sup.Player1WinSuper

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

winningBoard :: GameBoard
winningBoard =
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

winningState = PlayState winningBoard Player1

winningSuperBoardAllMiniBoardsSame :: Sup.SuperGameBoard
winningSuperBoardAllMiniBoardsSame =
        [
                [winningBoard, winningBoard, winningBoard],
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
                [winningBoard, winningBoard, winningBoard2],
                [startingBoard, startingBoard, startingBoard],
                [startingBoard, startingBoard, startingBoard]
        ]

winningSuperStateDifferentMiniBoards :: Sup.SuperGameState
winningSuperStateDifferentMiniBoards = Sup.SuperPlayState (2, 2) winningSuperBoardDifferentMiniBoards Player1

main = defaultMain tests
