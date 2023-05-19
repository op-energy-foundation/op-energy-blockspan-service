let
  pkgs0 = (import <nixpkgs> {}); # first, load the nixpkgs with system-wide overlays
  pkgs = pkgs0 // (import ./overlay-set.nix { pkgs = pkgs0; }); # second, add local packages into the scope of pkgs
  shell = pkgs.stdenv.mkDerivation {
    name = "shell";
    buildInputs
      = pkgs.op-energy-api.env.nativeBuildInputs
      ++ pkgs.op-energy-api.buildInputs
      ++ [ pkgs.haskellPackages.swagger2 pkgs.haskellPackages.servant pkgs.haskellPackages.servant-swagger
         ]
      ++ [ pkgs.haskellPackages.ghci pkgs.haskellPackages.ghcid pkgs.haskellPackages.cabal-install pkgs.swagger-codegen ]
      ++ [
        pkgs.jq
      ];
  };

in shell
