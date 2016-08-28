{ mkDerivation, base, bytestring, data-default, exceptions
, http-types, network, stdenv, streaming-commons, systemd
, transformers, unix, wai
}:
mkDerivation {
  pname = "warp-socket-activation";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring data-default exceptions http-types network
    streaming-commons systemd transformers unix wai
  ];
  description = "A simple wrapper for socket based activation";
  license = stdenv.lib.licenses.bsd3;
}
