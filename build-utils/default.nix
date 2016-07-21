{ mkDerivation, base, Cabal, directory, filepath, haskell-src-exts
, stdenv
}:
mkDerivation {
  pname = "xhb-requests-build-utils";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base Cabal directory filepath haskell-src-exts
  ];
  license = stdenv.lib.licenses.mit;
}
