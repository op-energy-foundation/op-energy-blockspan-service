{ GIT_COMMIT_HASH }: self: super:
let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/archive/0f8f64b54ed07966b83db2f20c888d5e035012ef.tar.gz";
  pkgs = import nixpkgs {
    config = {};
    overlays = [ ];
  };
  args1 = {
    GIT_COMMIT_HASH = GIT_COMMIT_HASH;
    pkgs = pkgs;
  };
  # this snapshot of packages is being used for building backend2
  backend2-nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/archive/4d2b37a84fad1091b9de401eb450aae66f1a741e.tar.gz";
  backend2-pkgs = import backend2-nixpkgs {
    config = {};
    overlays = [
    ];
  };
  backend2-args1 = {
    GIT_COMMIT_HASH = GIT_COMMIT_HASH;
    pkgs = backend2-pkgs;
  };

  # library, that maybe shared between backend and frontend
  op-energy-api = backend2-pkgs.haskellPackages.callPackage ../op-energy-api/op-energy-api.nix backend2-args1;
  op-energy-backend = backend2-pkgs.haskellPackages.callPackage ./derivation.nix (backend2-args1 // {
    op-energy-api = op-energy-api;
  });
in {
  op-energy-backend = op-energy-backend.bin;
}
