{ nixpkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/86191b5b91322bdd88303e31d4507a684fc1b120.tar.gz) {}
, ghc ? nixpkgs.haskell.compiler.ghc822Binary
}:

with nixpkgs;

let
  nativeLibs = [
    gmp
    postgresql
    redis
    zlib
  ] ++ lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
    Cocoa
    CoreServices
  ]);

in haskell.lib.buildStackProject {
  inherit ghc;

  name = "ted2srt";

  buildInputs = nativeLibs;
}
