Super Tic-tac-toe (and normal Tic-Tac-Toe) in Haskell
=====================================================

What it is:
-----------

Implementing Super tic-tac-toe as well as a game AI in Haskell! The game AI
uses a minimax algorithm to determine what moves to play.

Normal tic-tac-toe is included as well.

How to build:
-------------

The only prerequisites you'll need are GHC and Cabal for the main program. If
you want to the run the tests, you'll also need some of the other dependencies,
but these should be automatically handled by Cabal.

This repository comes with an extremely rudimentary Makefile which should
install the game to your local `.cabal\bin` folder just by running `make` at the
commandline (this will not install the tests).

Alternatively, you could manually invoke Cabal within the repository via

    cabal install

How to Run:
-----------

Just by running `tictactoe-haskell` after building, you should be able to see
the following.

    $ tictactoe-haskell
     | |
     | |
     | |

    Enter a move! Enter in tuple format, e.g. (1, 2).

Of course this is the boring normal version of tic-tac-toe. The super version
can be accessed by running `tictactoe-haskell -s`. Running it should yield the
following.

    $ tictactoe-haskell -s
     | | {} | | {} | |
     | | {} | | {} | |
     | | {} | | {} | |

     | | {} | | {} | |
     | | {} | | {} | |
     | | {} | | {} | |

     | | {} | | {} | |
     | | {} | | {} | |
     | | {} | | {} | |

    Current board is (1,1)

    Enter a move! Enter in tuple format, e.g. (1, 2).

To play against an AI, use the `--useAI` option. To view all the options
available, run `tictactoe-haskell --help`.

Running Tests
-------------

This game comes with a suite of tests. To run these tests, it's
first necessary to install some dependencies which the game itself does not
need. These can be done simply via

    cabal install --enable-tests

Thereafter, you can either run `make test` or

    cabal configure --enable-tests
    cabal build
    cabal test

to test the code (they both do the same thing).

There is also a pre-commit hook which can be used with Git to make sure that
before every commit, these tests are run and if they fail the commit will not
go through. It is located at `pre-commit-hook.sh` and a simple

    cp pre-commit-hook.sh .git/hooks/pre-commit-hook
    chmod +x .git/hooks/pre-commit-hook

should be sufficient to activate the hook. Note that the pre-commit hook is
based off of Git's default one and so includes the portion which checks for
trailing whitespace.
