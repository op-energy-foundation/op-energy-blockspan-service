let
  pkgs1 = import <nixpkgs> {
    config = {};
    overlays = [
    ];
  };
  sourceWithGit = pkgs1.copyPathToStore ../.;
  GIT_COMMIT_HASH = builtins.readFile ( # if git commit is empty, then try to get it from git
    pkgs1.runCommand "get-rev" {
      nativeBuildInputs = [ pkgs1.git ];
    } ''
      HASH=$(GIT_DIR=${sourceWithGit}/.git git rev-parse --short HEAD | tr -d '\n' || printf 'NOT A GIT REPO')
      printf $HASH > $out
    ''
  );
  overlay = (import ./overlay.nix) { GIT_COMMIT_HASH = GIT_COMMIT_HASH; };
  pkgs = import <nixpkgs> {
    config = {};
    overlays = [
      overlay
    ];
  };
in {
  op-energy-frontend = pkgs.op-energy-frontend;
}
