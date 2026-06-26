let
  stable = import ../nixpkgs.nix;
  pkgs = (import stable {
    overlays = [ ];
  }); # first, load the nixpkgs with system-wide overlays
  shell = pkgs.stdenv.mkDerivation {
    name = "shell";
    buildInputs
      = [ pkgs.python3 pkgs.wget pkgs.curl ]
      ;
  };

in shell
