{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, HUnit, optparse-applicative
      , QuickCheck, silently, stdenv, test-framework
      , test-framework-hunit, test-framework-quickcheck2, vty-ui
      , cabal-install
      }:
      mkDerivation {
        pname = "tictactoe-haskell";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base containers optparse-applicative vty-ui
        ];
        testHaskellDepends = [
          base containers HUnit optparse-applicative QuickCheck silently
          test-framework test-framework-hunit test-framework-quickcheck2
          vty-ui
        ];
        homepage = "www.shuangrimu.com";
        description = "A short tictactoe with a minmax AI";
        license = stdenv.lib.licenses.gpl3;
        buildTools = [ cabal-install ];
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
