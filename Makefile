all:
	cabal configure
	cabal build
	cabal install

clean:
	rm *.hi
	rm *.o
	rm Game
