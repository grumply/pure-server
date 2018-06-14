{ mkDerivation, ghc, base, containers
, stdenv
, pure-core, pure-default, pure-websocket
, secure ? false
}:
mkDerivation {
  pname = "pure-websocket";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers pure-core pure-default pure-websocket ];
  configureFlags = [ (secure ? "-fsecure") ];
  homepage = "github.com/grumply/pure-server";
  license = stdenv.lib.licenses.bsd3;
}
