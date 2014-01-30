Tic-tac-toe in Haskell
======================

What it is:
-----------

Implementing tic-tac-toe as well as a game AI in Haskell! The game AI uses a
minimax algorithm to determine what moves to play.

How to build:
-------------

The only prerequisites you'll need are GHC and Cabal for the main program. If
you want to the run the tests, you'll also need some of the other dependencies,
but these should be automatically handled by Cabal. From within the repository,
run `cabal configure --enable-tests && cabal build && cabal install
--enable-tests` to do this fully automatically.

This repository comes with an extremely rudimentary Makefile which should
install the game to your local `.cabal\bin` folder just by running `make` at the
commandline (this will not install the tests).

Alternatively, you could manually invoke Cabal within the repository via

    cabal configure
    cabal build
    cabal install

How to Run:
-----------

Just by running `tictactoe-haskell` after building, you should be able to see
the following.

    $ tictactoe-haskell

     | | 
     | | 
     | | 

    Enter a move!

Tada! You're done!
