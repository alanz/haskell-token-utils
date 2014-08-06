let pkgs = import <nixpkgs> {};
    haskellPackages = pkgs.haskellPackages.override {
      extension = self: super: {
        haskellTokenUtils = self.callPackage ./. {};
      };
    };
 in pkgs.lib.overrideDerivation haskellPackages.haskellTokenUtils (attrs: {
   buildInputs = [ haskellPackages.cabalInstall haskellPackages.ghcMod ] ++ attrs.buildInputs;
 })


