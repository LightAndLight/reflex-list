{ reflex-platform ? import ./nix/reflex-platform.nix }:
reflex-platform.project ({ pkgs, ... }: {
  packages = {
    reflex-list = ./reflex-list;
  };

  shells = {
    ghc = ["reflex-list"];
  };

  overrides = self: super: {
    reflex-basic-host =
      self.callPackage ./nix/reflex-basic-host.nix {};
  };
})
