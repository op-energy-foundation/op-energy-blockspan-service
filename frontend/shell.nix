let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/archive/0f8f64b54ed07966b83db2f20c888d5e035012ef.tar.gz";
  pkgs = import nixpkgs
    { };
  myTerraform = pkgs.terraform_0_15.withPlugins (tp: [ tp.digitalocean ]);
in
pkgs.mkShell {
  buildInputs = with pkgs; [ curl jq myTerraform python3 nodejs ];
}
