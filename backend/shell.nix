{ nixpkgs ? import ../nix/nixpkgs.nix {}
, ghc ? nixpkgs.ghc
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
