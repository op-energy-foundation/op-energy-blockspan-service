{GIT_COMMIT_HASH}:
let
  op-energy-api-overlay = import ./op-energy-api/overlay.nix;
  op-energy-backend-overlay = import ./op-energy-backend/overlay.nix {
    GIT_COMMIT_HASH = GIT_COMMIT_HASH;
  };
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/archive/4d2b37a84fad1091b9de401eb450aae66f1a741e.tar.gz";
  pkgs = import nixpkgs {
    config = {};
    overlays = [
      op-energy-api-overlay
      op-energy-backend-overlay
    ];
  };
  op-energy = {
    op-energy-api = pkgs.op-energy-api;
    op-energy-backend = pkgs.op-energy-backend;
  };
in
op-energy
