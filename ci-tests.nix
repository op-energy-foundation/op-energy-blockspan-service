{ GIT_COMMIT_HASH }:
let
  # Pin nixpkgs, see pinning tutorial for more details
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/archive/8c50a710ddca43d7a530fb805ad55bde8d0141c5.tar.gz";
  pkgs = import nixpkgs {};

  # Single source of truth for all tests
  apiPort       = 8999;

  # NixOS module shared between server and client
  sharedModule = {
    # Since it's common for CI not to have $DISPLAY available, we have to explicitly tell the tests "please don't expect any screen available"
    virtualisation.graphics = false;
  };
  env = {
    bitcoind-mainnet-rpc-pskhmac = "d4fcd97f7a2fed9bac806a88ce10408f$423300a76481a1b3189ec406381fed941e655c65d58f46c344a2fb729284a1f1";
    bitcoind-signet-rpc-pskhmac = "15d9d48ce024de5bcbecabac7e5498eb$fae7629913c424511c787a12b83dba9e1491ba6b7b7b89ad7e14e638e90f5dc4";
    GIT_COMMIT_HASH              = GIT_COMMIT_HASH;
  };

in pkgs.testers.nixosTest ({
  name = "ci-test";

  nodes = {
    server = args@{ config, pkgs, ... }: let
      sources = pkgs.copyPathToStore ./op-energy-dev-instance;
      op-energy-host = import ./op-energy-dev-instance/host.nix env;
    in {
      imports = [
        sharedModule
        op-energy-host
      ];
      networking.firewall.allowedTCPPorts = [ 8999 ];
      networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];

      users = {
        mutableUsers = false;
        users = {
          # For ease of debugging the VM as the `root` user
          root.password = "";
        };
      };

      # CI-only secrets are provided as files under /etc/nixos/private/ (the
      # credentials_locations defaults of the hardened modules).
      # those values are only for CI environment and are not used anywhere else
      environment.etc."nixos/private/OP_ENERGY_BLOCKSPANS_MAINNET_BTC_PASSWORD_SECRET" = {
        mode = "0400";
        text = "53d1321353b90780d1ba737730fb840588239a4abb7ad83c972770c3d2665f13";
      };
      environment.etc."nixos/private/OP_ENERGY_BLOCKSPANS_MAINNET_DB_PASSWORD_SECRET" = {
        mode = "0400";
        text = "b6d265eb3e31b605045621125c1f687a1825fe533c9c438450095b00358c870b";
      };
      environment.etc."nixos/private/OP_ENERGY_ACCOUNT_DB_PASSWORD_SECRET" = {
        mode = "0400";
        text = "9924f6a751ea9718a4b3b1a6d5151d8c08f603c09db3efcc630db2277e792c89";
      };
      environment.etc."nixos/private/OP_ENERGY_OFFER_DB_PASSWORD_SECRET" = {
        mode = "0400";
        text = "83a458220507ef9a0e2542f0b605cde7797acde35235656c4f11e98f8896c6d9";
      };
      environment.etc."nixos/private/OP_ENERGY_ACCOUNT_SECRET_SALT_SECRET" = {
        mode = "0400";
        text = "fb2025297c6b41a88797eaa1c68e49770d41cf9d96714ca8dd39008fe9f44cbd";
      };
      environment.etc."nixos/private/OP_ENERGY_ACCOUNT_TOKEN_ENCRYPTION_PRIVATE_KEY_SECRET" = {
        mode = "0400";
        text = "vA1e9POQx0PWnuWHv5BGak4uYQ2BblQuqfEB0VMZ/563m+0Xw9M55KLtXyElYSwgIT8EE6JnRUtO9mfhRotgZo3vAShv0BAchHGkHGUPRzNNADqNlJ2p25EcinNIoXQs";
      };
      environment.etc."nixos/private/INTERNAL_SERVICE_SHARED_SECRET" = {
        mode = "0400";
        text = "HS1l0+gV437Ha88GZ/DF4+XXMin765ABc6dP77CtDvk=";
      };
      environment.etc."nixos/private/LITD_UI_PASSWORD_SECRET" = {
        mode = "0400";
        text = "0c556dfa9bb94e9352013badf6f8902168f75b8d934334e4ea94b1080a86cce1";
      };
      environment.etc."nixos/private/OP_ENERGY_BLOCKSPANS_SIGNET_BTC_PASSWORD_SECRET" = {
        mode = "0400";
        text = "d24528e51954acde78036369b372e0510fd59ebe10cad913d0de100c13a03d29";
      };
      environment.etc."nixos/private/OP_ENERGY_BLOCKSPANS_SIGNET_DB_PASSWORD_SECRET" = {
        mode = "0400";
        text = "1d47aa35cf9a5d38eac6b51e25e7351fa2f23436832a654555b8aa7b52252428";
      };

    };

    client = {
      imports = [ sharedModule ];
    };
  };

  # Disable linting for simpler debugging of the testScript
  skipLint = true;

  testScript = ''
    import json
    import sys

    start_all()

    server.wait_for_open_port(${toString apiPort })

    # just needs to succed
    raw = client.succeed(
            "${pkgs.curl}/bin/curl http://ci-host:${toString apiPort}/api/v1/oe/git-hash"
        )
    print( raw)
  '';
})
