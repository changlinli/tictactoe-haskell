import qualified Graphics.Vty.Widgets.All as VA
import qualified Graphics.Vty as Vty
import qualified Tictactoe as Tic
import qualified Data.Text as DT
import qualified Control.Concurrent as CC

newtype Banner = Banner String deriving (Show)

newBanner :: String -> IO (VA.Widget Banner)
newBanner input = VA.newWidget state createWidgetLogic where
        state = Banner input
        createWidgetLogic defaultLogic =
                defaultLogic { VA.render_ = newRenderFunc }
        newRenderFunc this size context = do
                (Banner x) <- VA.getState this
                return $ Vty.string (VA.getNormalAttr context) x

newGameBoardWidget :: Tic.GameBoard -> IO (VA.Widget Tic.GameBoard)
newGameBoardWidget board = VA.newWidget state createWidgetLogic where
        state = board
        createWidgetLogic defaultLogic =
                defaultLogic { VA.render_ = newRenderFunc }
        newRenderFunc this size context = do
                gameBoard <- VA.getState this
                return $ Vty.string (VA.getNormalAttr context) (Tic.showGameBoard gameBoard)

playAI :: Tic.GameState -> VA.Widget VA.FormattedText -> IO ()
playAI Tic.Player1Win widget = VA.schedule $ (VA.setText widget (DT.pack "Player 1 Won!") >> VA.shutdownUi)
playAI Tic.Player2Win widget = VA.schedule $ (VA.setText widget (DT.pack "Player 2 Won!") >> VA.shutdownUi)
playAI Tic.Tie widget = VA.schedule $ (VA.setText widget (DT.pack "There was a tie!") >> VA.shutdownUi)
playAI state widget = do
        moveCoord <- return (Tic.findBestMove state)
        newState <- return $ Tic.playMove moveCoord state
        VA.schedule $ VA.setText widget (DT.pack (Tic.showGameBoard $ Tic.currentBoard newState))
        CC.threadDelay 1000000
        playAI (Tic.checkGameOver newState) widget

main :: IO ()
main = do
        gameStart <- return Tic.startingState
        gameBoardWidget <- VA.plainText (DT.pack $ Tic.showGameBoard (Tic.currentBoard gameStart))
        gameBoardWidget `VA.onKeyPressed` \_ key _ ->
                if key == Vty.KASCII 'q' then VA.shutdownUi >> return True
                                        else return False
        borderedGameBoard <- VA.bordered gameBoardWidget
        ui <- VA.centered borderedGameBoard
        fg <- VA.newFocusGroup
        VA.addToFocusGroup fg gameBoardWidget
        c <- VA.newCollection
        _ <- VA.addToCollection c ui fg
        CC.forkIO $ playAI gameStart gameBoardWidget
        VA.runUi c VA.defaultContext
        putStrLn "Now we're done!"
