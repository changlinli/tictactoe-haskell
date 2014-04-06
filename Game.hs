{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified Tictactoe as Tic
import qualified SuperTictactoe as Sup
import qualified Options.Applicative as OA
import qualified Data.Maybe as DM

data PossibleFlags = Flags{superGame :: Bool, useAI :: String}

flagParser :: OA.Parser PossibleFlags
flagParser = Flags
        OA.<$> OA.switch
                ( OA.long "super"
                OA.<> OA.short 's'
                OA.<> OA.help "Play Super Tic-Tac-Toe instead of normal Tic-Tac-Toe" )
        OA.<*> OA.strOption
                ( OA.long "useAI"
                OA.<> OA.short 'a'
                OA.<> OA.metavar "AIOPTION"
                OA.<> OA.value "0"
                OA.<> OA.help "A number between 0 and 3 (inclusive) to decide whether we use an AI. 0 (the default) has two human players, 1 pits a human first player against a computer second player, 2 pits a computer first player against a human second player, and 3 pits two computers against each other." )

main :: IO ()
main = do
        Flags superFlag useAINum <- OA.execParser opts
        let gameType = Tic.maybeRead useAINum :: Maybe Int
        if not superFlag && optsMakeSense gameType
           then do Tic.playGameAI (DM.fromJust gameType) Tic.startingState
           else if superFlag && optsMakeSense gameType
                then do Sup.playGameAI (DM.fromJust gameType) Sup.startingSuperState
                else do
                putStrLn "It looks like you may have made some errors in the useAI option. Usually this is because you used an invalid number. Run this program with the --help option for more details."
        where
                opts = OA.info (OA.helper OA.<*> flagParser)
                        ( OA.fullDesc
                        OA.<> OA.progDesc "Select which game of Tic-tac-toe you would like to play."
                        OA.<> OA.header "Tic-tac-toe with both its normal and its super (and harder) variant.")
                optsMakeSense x = DM.isJust x && DM.fromJust x <= 3 && DM.fromJust x >= 0
