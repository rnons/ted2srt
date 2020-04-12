{ nixpkgs ? import ../nix/nixpkgs.nix {}
, compiler ? "ghc864"
}:

let
  overrides = import ../nix/all.nix { inherit nixpkgs compiler; };
in
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./ted2srt.nix overrides
