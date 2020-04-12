{ nixpkgs ? import ./nixpkgs.nix {}
, compiler ? "ghc864"
}:

let
  global = { inherit nixpkgs compiler; };

in {
  with-utf8 = import ./pkgs/with-utf8.nix global;
}
