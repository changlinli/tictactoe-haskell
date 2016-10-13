{-# LANGUAGE FlexibleInstances, CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans
-fno-warn-unused-binds #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Control.Monad
#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif
import Test.QuickCheck
import Test.HUnit
import Negamax
import System.IO.Silently
import Tictactoe as Tic
import qualified SuperTictactoe as Sup

instance Arbitrary (ExtendedNum Integer) where
        arbitrary = oneof [integerGen, infGen] where
                integerGen = fmap Only (choose (0 :: Integer, 1000000000000000000 :: Integer))
                infGen = elements [PosInf, NegInf]

instance Arbitrary Tic.Players where
        arbitrary = elements [Player1, Player2]

newtype NewGameBoard = NewGameBoard Tic.GameBoard

unNewGameBoard :: NewGameBoard -> Tic.GameBoard
unNewGameBoard (NewGameBoard x) = x

instance Eq NewGameBoard where
        (==) x y = (==) (unNewGameBoard x) (unNewGameBoard y)

instance Show NewGameBoard where
        show = show . unNewGameBoard

instance Arbitrary NewGameBoard where
        arbitrary = fmap NewGameBoard ((replicateM 3 . replicateM 3) (arbitrary :: Gen GameBoardUnit))

prop_AbsSignum :: ExtendedNum Integer -> Bool
prop_AbsSignum x = abs x * signum x == x

prop_checkFullNoValidMoves :: ((Int, Int), NewGameBoard) -> Bool
prop_checkFullNoValidMoves (randMove, randBoard) = if checkFull (unNewGameBoard randBoard)
                                                      then not (isValidMove randMove (unNewGameBoard randBoard))
                                                      else True

prop_ExtendedNumObeysIdFunctorLaw :: ExtendedNum Integer -> Bool
prop_ExtendedNumObeysIdFunctorLaw x = fmap id x == id x

tests =
        [
        testGroup "Negamax Tests"
                [
                        testProperty "abs and signum satisfies law specified in Num" prop_AbsSignum,
                        testProperty "ExtendedNum obeys the functor law `fmap id = id`" prop_ExtendedNumObeysIdFunctorLaw
                ]
        , testGroup "TicTacToe Tests"
                [
                        testCase "Player 1 finds winning move" test_1,
                        testCase "Player 2 finds winning move" test_2,
                        testCase "Player 2 finds move to block player 1 win" test_3,
                        testCase "Player 1 finds move to block player 2 win" test_4,
                        testCase "isValidMove rejects inputs that imply moves with negative index" test_10,
                        testCase "isValidMove rejects inputs that imply moves with too high of an index" test_11,
                        testProperty "checkFull board == True implies that isValidMove will be false for any move on that board" prop_checkFullNoValidMoves
                ]
        , testGroup "SuperTicTacToe Tests"
                [
                        testCase "Game recognizes Player 1 victory when all top rows are won and are the same mini board" test_5,
                        testCase "Game recognizes Player 1 victory when all top rows are won, but are different mini boards" test_6,
                        testCase "findBestMove AI as player 1 finds winning move" test_7,
                        testCase "findBestMove AI as player 2 finds winning move" test_8,
                        testCase "Game recognizes Player 2 victory when left column is won, but are different mini boards" test_9,
                        testCase "isValidMove rejects inputs that imply moves with negative index" test_12,
                        testCase "isValidMove rejects inputs that imply moves with too high of an index" test_13,
                        testCase "isValidSuperMove rejects an input which sends us to a board that is already full" test_14,
                        testCase "checkAnyValidMove rejects a state in all possible moves send us to a board that is already full" test_15,
                        testCase "superAI can make at least one move from a given state" test_16,
                        testCase "superAI can make move from beginning" test_17
                ]
        ]

test_1 = Tic.findBestMove nearlyWinningState1 @?= (2, 0)

test_2 = Tic.findBestMove nearlyWinningState2 @?= (0, 1)

test_3 = Tic.findBestMove nearlyLosingState2 @?= (2, 2)

test_4 = Tic.findBestMove nearlyLosingState1 @?= (2, 1)

test_5 = Sup.checkSuperGameOver winningSuperStateAllMiniBoardsSame @?= Sup.Player1WinSuper

test_6 = Sup.checkSuperGameOver winningSuperStateDifferentMiniBoards @?= Sup.Player1WinSuper

test_7 = Sup.findBestMove nearlyWinningSuperState1 Sup.maximumDepth @?= (2, 0)

test_8 = Sup.findBestMove nearlyWinningSuperState2 Sup.maximumDepth @?= (0, 1)

test_9 = Sup.checkSuperGameOver winningSuperState2 @?= Sup.Player2WinSuper

test_10 = Tic.isValidMove (-1, 0) Tic.startingBoard || Tic.isValidMove (0, -1) Tic.startingBoard || Tic.isValidMove (-2, -3) Tic.startingBoard @?= False

test_11 = Tic.isValidMove (3, 2) Tic.startingBoard || Tic.isValidMove (3, 2) Tic.startingBoard || Tic.isValidMove (3, 3) Tic.startingBoard @?= False

test_12 = Sup.isValidSuperMove (-1, 0) Sup.startingSuperState || Sup.isValidSuperMove (0, -1 ) Sup.startingSuperState || Sup.isValidSuperMove (-2, -3) Sup.startingSuperState @?= False

test_13 = Sup.isValidSuperMove (3, 2) Sup.startingSuperState || Sup.isValidSuperMove (3, 2) Sup.startingSuperState || Sup.isValidSuperMove (3, 3) Sup.startingSuperState @?= False

test_14 = Sup.isValidSuperMove (1, 1) fullCenterMiniBoardSuperState @?= False

test_15 = Sup.checkAnyValidSuperMoves allMiniBoardsFullExceptOneSuperState @?= False

test_16 = assert (fmap ((==) True)
        (
                 (fmap ((/=) Sup.TieSuper) nextState) *||*
                 (fmap ((/=) Sup.Player1WinSuper) nextState) *||*
                 (fmap ((/=) Sup.Player2WinSuper) nextState) *||*
                 (fmap ((/=) nearlyFullSuperBoardSuperState) nextState)
        ))
        where nextState = silence $ Sup.playAIMove nearlyFullSuperBoardSuperState 4
              x *||* y = (||) <$> x <*> y
              infixr 2 *||*

test_17 = assert (fmap ((==) True)
        (
                 (fmap ((/=) Sup.TieSuper) nextState) *&&*
                 (fmap ((/=) Sup.Player1WinSuper) nextState) *&&*
                 (fmap ((/=) Sup.Player2WinSuper) nextState) *&&*
                 (fmap ((/=) Sup.startingSuperState) nextState)
        ))
        where nextState = silence $ Sup.playAIMove Sup.startingSuperState 2
              x *&&* y = (&&) <$> x <*> y
              infixr 2 *&&*

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

miniBoardOneElement :: Tic.GameBoard
miniBoardOneElement =
        [
                [Nothing, Nothing, Nothing],
                [Nothing, Just Player1, Nothing],
                [Nothing, Nothing, Nothing]
        ]

miniBoardFullElementWithCenterElement :: Tic.GameBoard
miniBoardFullElementWithCenterElement =
        [
                [Just Player2, Just Player2, Just Player2],
                [Just Player2, Just Player1, Just Player2],
                [Just Player2, Just Player2, Just Player2]
        ]

fullCenterMiniBoardSuperBoard :: Sup.SuperGameBoard
fullCenterMiniBoardSuperBoard =
        [
                [Tic.startingBoard, miniBoardOneElement, miniBoardOneElement],
                [miniBoardOneElement, miniBoardFullElementWithCenterElement, miniBoardOneElement],
                [miniBoardOneElement, miniBoardOneElement, miniBoardOneElement]
        ]

fullCenterMiniBoardSuperState :: Sup.SuperGameState
fullCenterMiniBoardSuperState = Sup.SuperPlayState (0, 0) fullCenterMiniBoardSuperBoard Tic.Player1

topLeftTakenMiniBoard :: Tic.GameBoard
topLeftTakenMiniBoard =
        [
                [Just Player1, Nothing, Nothing],
                [Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing]
        ]

allMiniBoardsFullExceptOne :: Sup.SuperGameBoard
allMiniBoardsFullExceptOne =
        [
                [topLeftTakenMiniBoard, fullMiniBoardTie, fullMiniBoardTie],
                [fullMiniBoardTie, fullMiniBoardTie, fullMiniBoardTie],
                [fullMiniBoardTie, fullMiniBoardTie, fullMiniBoardTie]
        ]

allMiniBoardsFullExceptOneSuperState :: Sup.SuperGameState
allMiniBoardsFullExceptOneSuperState = Sup.SuperPlayState (0, 0) allMiniBoardsFullExceptOne Tic.Player1

nearlyFullSuperBoard :: Sup.SuperGameBoard
nearlyFullSuperBoard =
        [
                [Tic.startingBoard, Tic.startingBoard, fullMiniBoardTie],
                [fullMiniBoardTie, fullMiniBoardTie, fullMiniBoardTie],
                [fullMiniBoardTie, fullMiniBoardTie, fullMiniBoardTie]
        ]

nearlyFullSuperBoardSuperState = Sup.SuperPlayState (0, 0) nearlyFullSuperBoard Tic.Player1

main = defaultMain tests
