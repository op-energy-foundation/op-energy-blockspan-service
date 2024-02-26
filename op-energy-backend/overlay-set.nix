{ GIT_COMMIT_HASH }:
{ pkgs
, ...
}:
let
  op-energy-backend = pkgs.haskellPackages.callPackage ./derivation.nix { GIT_COMMIT_HASH = GIT_COMMIT_HASH; };
in {
  op-energy-backend = op-energy-backend;
}
