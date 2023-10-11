let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/archive/4d2b37a84fad1091b9de401eb450aae66f1a741e.tar.gz";
  pkgs = import nixpkgs
    { };
  myTerraform = pkgs.terraform.withPlugins (tp: [ tp.digitalocean ]);
in
pkgs.mkShell {
  buildInputs = with pkgs; [ curl jq myTerraform python3 nodejs ];
}
