{ mkDerivation, aeson, attoparsec, base, bloomfilter
, bytestring, containers, directory
, filepath, http-conduit, http-types, optparse-applicative
, stdenv, stringsearch, text, transformers, lib
}:
mkDerivation {
  pname = "sloane";
  version = "5.0.2";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson attoparsec base bloomfilter bytestring
    containers directory filepath http-conduit http-types
    optparse-applicative stringsearch text transformers
  ];
  homepage = "http://akc.is/sloane";
  description = "A command line interface to Sloane's OEIS";
  license = lib.licenses.bsd3;
}
