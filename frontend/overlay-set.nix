{ GIT_COMMIT_HASH}:
{ pkgs ? {}
, ...
}:

let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/archive/0f8f64b54ed07966b83db2f20c888d5e035012ef.tar.gz";
  args1 = {
    GIT_COMMIT_HASH = GIT_COMMIT_HASH;
    pkgs = pkgs;
  };
in {
  op-energy-frontend = (pkgs.callPackage ./derivation.nix args1).op-energy-frontend;
}
