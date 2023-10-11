let
  overlay = import ../overlay.nix { GIT_COMMIT_HASH = "";};
  pkgs = (import <nixpkgs> {
    overlays = [ overlay ];
  }); # first, load the nixpkgs with system-wide overlays
  shell = pkgs.stdenv.mkDerivation {
    name = "shell";
    buildInputs
      = pkgs.op-energy-client.nativeBuildInputs
      ++ pkgs.op-energy-client.buildInputs
      ++ [ pkgs.swagger-codegen ]
      ++ [
        pkgs.jq
      ];
  };

in shell
