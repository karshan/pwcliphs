# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ DRBG, haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
  let 
    inherit (haskellPackages) cabal dataDefault handaGdata;
in cabal.mkDerivation (self: {
  pname = "pwcliphs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ dataDefault DRBG handaGdata ];
  meta = {
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
