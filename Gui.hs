import qualified Graphics.Vty.Widgets.All as VA
import qualified Graphics.Vty as Vty
import qualified Tictactoe as Tic
import qualified SuperTictactoe as Sup
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

newSuperGameWidget :: Sup.SuperGameState -> IO (VA.Widget Sup.SuperGameState)
newSuperGameWidget board = VA.newWidget state createWidgetLogic where
        state = board
        createWidgetLogic defaultLogic =
                defaultLogic { VA.render_ = newRenderFunc }
        newRenderFunc this size context = do
                superGameState <- VA.getState this
                return $ Vty.string (VA.getNormalAttr context) (Sup.showSuperGameBoard . Sup.currentSuperBoard $ superGameState)

playGameVty :: Int -> Tic.GameState -> VA.Widget VA.FormattedText -> IO ()
playGameVty _ Tic.Player1Win widget = VA.schedule $ (VA.setText widget (DT.pack "Player 1 Won!"))
playGameVty _ Tic.Player2Win widget = VA.schedule $ (VA.setText widget (DT.pack "Player 2 Won!"))
playGameVty _ Tic.Tie widget = VA.schedule $ (VA.setText widget (DT.pack "There was a tie!"))
playGameVty 3 state widget = do
        moveCoord <- return (Tic.findBestMove state)
        newState <- return $ Tic.playMove moveCoord state
        VA.schedule $ VA.setText widget (DT.pack (Tic.showGameBoard $ Tic.currentBoard newState))
        CC.threadDelay 1000000
        playGameVty 3 (Tic.checkGameOver newState) widget

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
        collection <- VA.newCollection
        _ <- VA.addToCollection collection ui fg
        CC.forkIO $ playGameVty 3 gameStart gameBoardWidget
        VA.runUi collection VA.defaultContext
        putStrLn "Now we're done!"
