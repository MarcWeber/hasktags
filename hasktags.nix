{ mkDerivation, base, bytestring, directory, filepath, HUnit, json
, stdenv, utf8-string
}:
mkDerivation {
  pname = "hasktags";
  version = "0.70.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring directory filepath json utf8-string
  ];
  executableHaskellDepends = [ base directory filepath ];
  testHaskellDepends = [
    base bytestring directory filepath HUnit json utf8-string
  ];
  homepage = "http://github.com/MarcWeber/hasktags";
  description = "Produces ctags \"tags\" and etags \"TAGS\" files for Haskell programs";
  license = stdenv.lib.licenses.bsd3;
}
