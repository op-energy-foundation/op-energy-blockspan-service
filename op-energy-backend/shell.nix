let
  overlay = import ../overlay.nix { GIT_COMMIT_HASH = "";};
  pkgs = (import <nixpkgs> {
    overlays = [ overlay ];
  }); # first, load the nixpkgs with system-wide overlays
  shell = pkgs.stdenv.mkDerivation {
    name = "shell";
    buildInputs
      = pkgs.op-energy-backend.env.nativeBuildInputs
      ++ pkgs.op-energy-backend.buildInputs
      ++ [ pkgs.haskellPackages.swagger2 pkgs.haskellPackages.servant pkgs.haskellPackages.servant-swagger
         ]
      ++ [ pkgs.haskellPackages.ghci pkgs.haskellPackages.ghcid pkgs.haskellPackages.cabal-install ];
  };

in shell
