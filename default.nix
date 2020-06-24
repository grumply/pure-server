{ mkDerivation, ghc, base, containers
, stdenv
, pure-core, pure-default, pure-websocket
}:
mkDerivation {
  pname = "pure-websocket";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers pure-core pure-default pure-websocket ];
  homepage = "github.com/grumply/pure-server";
  license = stdenv.lib.licenses.bsd3;
}
