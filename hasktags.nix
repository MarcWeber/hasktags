{ mkDerivation, base, bytestring, directory, filepath, HUnit, json
, microlens-platform, stdenv, utf8-string, optparse-applicative, containers
}:
mkDerivation {
  pname = "hasktags";
  version = "0.70.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring directory filepath json microlens-platform
    utf8-string optparse-applicative containers
  ];
  executableHaskellDepends = [ base directory filepath ];
  testHaskellDepends = [
    base bytestring directory filepath HUnit json microlens-platform
    utf8-string
  ];
  homepage = "http://github.com/MarcWeber/hasktags";
  description = "Produces ctags \"tags\" and etags \"TAGS\" files for Haskell programs";
  license = stdenv.lib.licenses.bsd3;
}
