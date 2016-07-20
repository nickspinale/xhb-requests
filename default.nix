{ nixpkgs ? import <nixpkgs> {}, compiler ? null }:

with nixpkgs;

let hp = if compiler == null
          then nixpkgs.haskellPackages
          else nixpkgs.haskell.packages.${compiler};

in rec {

  xhb-src = {
    version = "0.6.2015.8.1";
    src = fetchurl {
      url = https://hackage.haskell.org/package/xhb-0.6.2015.8.1/xhb-0.6.2015.8.1.tar.gz;
      sha256 = "1rq6g96v1fs66kdmnkvlkcxrv614ha77czclm3sfw274f7q2r2kb";
    };
  };

  xhb-monad-build-utils = hp.callPackage ./build-utils {};

  xhb-monad-src = stdenv.mkDerivation {

    name = "xhb-monad-src";
    version = xhb-monad-build-utils.version + "." + xhb-src.version;

    copyFiles = [./src ./README.md ./LICENSE];

    buildUtils = xhb-monad-build-utils;
    xhbSrc = xhb-src.src;

    builder = builtins.toFile "xhb-monad-src-builder" ''
      source $stdenv/setup
      mkdir $out
      for f in $copyFiles; do
        cp -r $f $out/$(echo $f | cut -d - -f 2-)
      done
      tar -xzf $xhbSrc
      $buildUtils/bin/gen-xhb-monad xhb-* $out
    '';

  };

  xhb-monad =
    let f = { mkDerivation, base, stdenv, xhb }: mkDerivation {
              pname = "xhb-monad";
              version = xhb-monad-src.version;
              src = xhb-monad-src;
              libraryHaskellDepends = [ base xhb ];
              license = stdenv.lib.licenses.mit;
            };
    in hp.callPackage f {};

}
