import <nixpkgs> {
  config = { pkgs }: {
    haskellPackageOverrides = self: super: with pkgs.haskell.lib; {
      xhb = appendPatch super.xhb ./xhb.patch;
      xhb-requests = self.callPackage ./xhb-requests.nix {};
      xhb-requests-src = self.callPackage ./xhb-requests-src.nix {};
      xhb-requests-build-utils = self.callPackage ./build-utils {};
    };
  };
}
