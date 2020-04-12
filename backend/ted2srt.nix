{ mkDerivation, aeson, base, base64-bytestring, bytestring, conduit
, conduit-extra, containers, cryptonite, cryptonite-conduit
, directory, either, hedis, hpack, html-conduit, http-conduit
, http-types, load-env, lucid, memory, monad-logger, mtl, network
, persistent, persistent-postgresql, persistent-template
, raw-strings-qq, regex-posix, rio, servant-lucid, servant-server
, stdenv, system-filepath, text, time, transformers
, unordered-containers, vector, wai, wai-extra, warp, with-utf8
, xml-conduit
, postgresql_11}:
mkDerivation {
  pname = "ted2srt";
  version = "3.20200412";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring bytestring conduit conduit-extra
    containers cryptonite cryptonite-conduit directory either hedis
    html-conduit http-conduit http-types load-env lucid memory
    monad-logger mtl network persistent persistent-postgresql
    persistent-template raw-strings-qq regex-posix rio servant-lucid
    servant-server system-filepath text time transformers
    unordered-containers vector wai with-utf8 xml-conduit
  ];
  librarySystemDepends = [ postgresql_11 ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base base64-bytestring bytestring conduit conduit-extra
    containers cryptonite cryptonite-conduit directory either hedis
    html-conduit http-conduit http-types load-env lucid memory
    monad-logger mtl network persistent persistent-postgresql
    persistent-template raw-strings-qq regex-posix rio servant-lucid
    servant-server system-filepath text time transformers
    unordered-containers vector wai wai-extra warp with-utf8
    xml-conduit
  ];
  doHaddock = false;
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
