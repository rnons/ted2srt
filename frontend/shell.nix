{ pkgs ? import ../nix/nixpkgs.nix {}
, nonbili ? import (fetchTarball https://github.com/nonbili/nonbili-nix-deps/archive/4fc735c10a8eee4a5d044164621651b89d0d6782.tar.gz) { inherit pkgs; }
}:

pkgs.mkShell {
  buildInputs = [
    nonbili.purs
    nonbili.spago
    nonbili.zephyr
  ];
}
