{ mkDerivation, base, bytestring, containers, directory, filepath
, HUnit, json, lib, microlens-platform, optparse-applicative
, utf8-string
}:
mkDerivation {
  pname = "hasktags";
  version = "0.71.2";
  sha256 = "1s2k9qrgy1jily96img2pmn7g35mwnnfiw6si3aw32jfhg5zsh1c";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring directory filepath json microlens-platform
    utf8-string
  ];
  executableHaskellDepends = [
    base containers directory filepath optparse-applicative
  ];
  testHaskellDepends = [
    base bytestring directory filepath HUnit json microlens-platform
    utf8-string
  ];
  homepage = "http://github.com/MarcWeber/hasktags";
  description = "Produces ctags \"tags\" and etags \"TAGS\" files for Haskell programs";
  license = lib.licenses.bsd3;
}
