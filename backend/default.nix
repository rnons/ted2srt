{ nixpkgs ? import ../nix/nixpkgs.nix {}
, compiler ? "ghc864"
}:

nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./ted2srt.nix {}
