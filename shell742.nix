let pkgs = import <nixpkgs> {};
    haskellPackages = pkgs.haskellPackages_ghc742.override {
      extension = self: super: {
        haskellTokenUtils = self.callPackage ./. {};
      };
    };
 in pkgs.lib.overrideDerivation haskellPackages.haskellTokenUtils (attrs: {
   buildInputs = [ haskellPackages.cabalInstall haskellPackages.ghcMod ] ++ attrs.buildInputs;
 })


