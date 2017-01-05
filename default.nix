{ pkgs ? (import <nixpkgs> {}).pkgs, compiler ? null }:

with pkgs;
let

  hp = if compiler == null
       then pkgs.haskellPackages
       else pkgs.haskell.packages.${compiler};

in rec {

  xhb-requests-build-utils = hp.callPackage ./build-utils {};

  xhb-requests-src = callPackage ./xhb-requests-src.nix {
    inherit xhb-requests-build-utils;
  };

  xhb-requests = hp.callPackage ./xhb-requests.nix {
    inherit xhb-requests-src;
  };

}
