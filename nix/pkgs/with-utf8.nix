{ nixpkgs
, compiler
, callPackage ? nixpkgs.pkgs.haskell.packages.${compiler}.callPackage
}:
let with-utf8 = callPackage
  ({ mkDerivation, base, deepseq, hedgehog, HUnit, safe-exceptions
    , tasty, tasty-discover, tasty-hedgehog, tasty-hunit, temporary
    , text, unix, stdenv
    }:
    mkDerivation {
      pname = "with-utf8";
      version = "1.0.0.0";
      sha256 = "06xznaszw7d6rznvzhzw3y4z31b4vx4djms85rq4qsbpfbdrh2zc";
      libraryHaskellDepends = [ base safe-exceptions text ];
      testHaskellDepends = [
        base deepseq hedgehog HUnit safe-exceptions tasty tasty-hedgehog
        tasty-hunit temporary text unix
      ];
      testToolDepends = [ tasty-discover ];
      description = "Get your IO right on the first try";
      license = stdenv.lib.licenses.mpl20;
    }) {};
in nixpkgs.pkgs.haskell.lib.dontHaddock
  (nixpkgs.pkgs.haskell.lib.dontCheck with-utf8)
