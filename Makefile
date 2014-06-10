all:
	cabal configure
	cabal build
	cabal install

clean:
	-rm *.hi
	-rm *.o
	-rm Game
	-rm Gui

test:
	cabal configure --enable-tests
	cabal build
	cabal test
