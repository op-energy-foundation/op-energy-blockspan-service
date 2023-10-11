{ pkgs ? {}
, ...
}:
let
in {
  op-energy-client = pkgs.callPackage ./derivation.nix {};
}
