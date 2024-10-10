{ mkDerivation, lib, base
, hspec, text
, servant, servant-swagger, swagger2
, servant-websockets
, aeson
, bytestring
, lens
, scientific
, cryptohash-sha256, base16-bytestring
, persistent, persistent-template
, random
, cereal
, http-media
, http-client
, http-client-tls
, servant-client
, data-default
, ...
}:
mkDerivation {
  pname = "op-energy-api";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  libraryHaskellDepends = [
    base
    servant servant-swagger swagger2
    servant-websockets
    aeson
    text bytestring
    lens
    scientific
    cryptohash-sha256 base16-bytestring
    random
    persistent persistent-template
    cereal
    http-media
    http-client
    http-client-tls
    servant-client
    data-default
  ];
  executableHaskellDepends = [
    base
    servant-swagger swagger2
    aeson
    bytestring
  ];
  enableLibraryProfiling = false;
  enableExecutableProfiling = false;
  enableSharedExecutables = false;
  enableSharedLibraries = true;
  enableLibraryForGhci = true;
  enableSeparateBinOutput = false;
  testHaskellDepends = [ base hspec text ];
  doBenchmark = false;
  doCheck = false;
  license = lib.licenses.bsd3;
}
