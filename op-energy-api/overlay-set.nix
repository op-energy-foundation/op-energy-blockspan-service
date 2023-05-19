{pkgs ? {}}:
let
  op-energy-api = pkgs.haskellPackages.callPackage ./op-energy-api.nix {};
in
{
  op-energy-api = op-energy-api;
}
