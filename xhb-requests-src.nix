{ stdenv, fetchurl, xhb-requests-build-utils }:

stdenv.mkDerivation rec {

  name = "xhb-requests-src";
  version = xhb-requests-build-utils.version + "." + xhbVersion;

  copyFiles = [./src ./README.md ./LICENSE];

  buildUtils = xhb-requests-build-utils;

  xhbVersion = "0.6.2015.8.1";

  xhbSrc = fetchurl {
    url = "https://hackage.haskell.org/package/xhb-${xhbVersion}pbcopy/xhb-${xhbVersion}.tar.gz";
    sha256 = "1rq6g96v1fs66kdmnkvlkcxrv614ha77czclm3sfw274f7q2r2kb";
  };

  builder = builtins.toFile "xhb-requests-src-builder" ''
    source $stdenv/setup
    mkdir $out
    for f in $copyFiles; do
      cp -r $f $out/$(echo $f | cut -d - -f 2-)
    done
    tar -xzf $xhbSrc
    $buildUtils/bin/gen-xhb-requests xhb-* $out
  '';

}
