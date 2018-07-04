{ mkDerivation, aeson, ansi-terminal, attoparsec, base, bloomfilter
, bytestring, conduit, conduit-extra, containers, directory
, filepath, http-conduit, http-types, optparse-applicative
, resourcet, stdenv, stringsearch, text, transformers
}:
mkDerivation {
  pname = "sloane";
  version = "5.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson ansi-terminal attoparsec base bloomfilter bytestring conduit
    conduit-extra containers directory filepath http-conduit http-types
    optparse-applicative resourcet stringsearch text transformers
  ];
  homepage = "http://akc.is/sloane";
  description = "A command line interface to Sloane's OEIS";
  license = stdenv.lib.licenses.bsd3;
}
