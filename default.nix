{ pkgs ? (import <nixpkgs> {}).pkgs, compiler ? null }:

let

  inherit pkgs;

  hp = if compiler == null
       then pkgs.haskellPackages
       else pkgs.haskell.packages.${compiler};

in with pkgs; rec {

  xhb-src = {
    version = "0.6.2015.8.1";
    src = fetchurl {
      url = https://hackage.haskell.org/package/xhb-0.6.2015.8.1/xhb-0.6.2015.8.1.tar.gz;
      sha256 = "1rq6g96v1fs66kdmnkvlkcxrv614ha77czclm3sfw274f7q2r2kb";
    };
  };

  xhb-requests-build-utils = hp.callPackage ./build-utils {};

  xhb-requests-src = stdenv.mkDerivation {

    name = "xhb-requests-src";
    version = xhb-requests-build-utils.version + "." + xhb-src.version;

    copyFiles = [./src ./README.md ./LICENSE];

    buildUtils = xhb-requests-build-utils;
    xhbSrc = xhb-src.src;

    builder = builtins.toFile "xhb-requests-src-builder" ''
      source $stdenv/setup
      mkdir $out
      for f in $copyFiles; do
        cp -r $f $out/$(echo $f | cut -d - -f 2-)
      done
      tar -xzf $xhbSrc
      $buildUtils/bin/gen-xhb-requests xhb-* $out
    '';

  };

  xhb-requests =
    let f = { mkDerivation, base, stdenv, xhb }: mkDerivation {
              pname = "xhb-requests";
              version = xhb-requests-src.version;
              src = xhb-requests-src;
              libraryHaskellDepends = [ base xhb ];
              license = stdenv.lib.licenses.mit;
            };
    in hp.callPackage f {};

}
