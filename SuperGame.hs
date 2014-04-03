import qualified Tictactoe as Tic
import SuperTictactoe

data PossibleFlags = Flags{superGame :: Bool, useAI :: Bool, player1Or2AI :: Integer}

main :: IO ()
main = do
        putStrLn "Please enter 0 for human vs human and another number for human vs AI"
        stringGameType <- getLine
        let gameType = read stringGameType :: Int
        if gameType == 0
           then playGame startingSuperState
           else playGameAI startingSuperState
