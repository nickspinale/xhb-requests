{ mkDerivation, base, stdenv, xhb, xhb-requests-src }:
mkDerivation {
  pname = "xhb-requests";
  version = xhb-requests-src.version;
  src = xhb-requests-src;
  libraryHaskellDepends = [ base xhb ];
  license = stdenv.lib.licenses.mit;
}
