hpack
cabal2nix --no-haddock . > ted2srt.nix
sed -i '0,/}/ s/}/, postgresql_11}/' ted2srt.nix
sed -i '0,/.*libraryToolDepends.*/ s/.*libraryToolDepends.*/  librarySystemDepends = [ postgresql_11 ];\n&/' ted2srt.nix
