let
  overlay = import ../overlay.nix { GIT_COMMIT_HASH = "";};
  pkgs = (import <nixpkgs> {
    overlays = [ overlay ];
  }); # first, load the nixpkgs with system-wide overlays

in pkgs.op-energy-client
