{GIT_COMMIT_HASH}:
let
  op-energy-api-overlay = import ./op-energy-api/overlay.nix;
  op-energy-backend-overlay = import ./op-energy-backend/overlay.nix {
    GIT_COMMIT_HASH = GIT_COMMIT_HASH;
  };
  stable = import ./nixpkgs.nix;
  pkgs = import stable {
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
