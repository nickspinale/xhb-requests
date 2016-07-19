{ mkDerivation, base, stdenv, xhb }:
mkDerivation {
  pname = "xhb-monad";
  version = "1.0.6.2015.8.1";
  src = ./.;
  libraryHaskellDepends = [ base xhb ];
  license = stdenv.lib.licenses.mit;
}
