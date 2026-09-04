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
  op-energy-db-psk-mainnet = "91794224aff99af7f36ee0bb8f210cc99242b15803651fffe68adca52b191416";
  op-energy-db-salt-mainnet = "a9a5fc93af271ae890a28d3cf0f056c8a07e8e6d0326913909707c319fd890d4";
  bitcoind-mainnet-rpc-psk = "bcb61e8b0a2c6f998e9996ae1d9da4d650b44a91150d8650be6a9e5c0b67d2d4";
  op-energy-account-token-encryption-key = "gzHQ1xTkFevavJ3xxd1fLA4PPxa7rQFXvivEFeEMNfYz99e4WBrywVJEnE/7KGrRYKvkSrSWh5tN/ZsE3cqKj4L57Vhm+hRyoR4oXFxtMVr9Tef2axGSijxQFvp6hocE";
  op-energy-internal-service-shared-secret = "mEVAVgAB1LJ1FO93ZL6cdWGp0dnUc9jtQ3zAS785uxjvkV1H7r2GnwTlRUbFGRScQc3QqDun/4X+PkDlci9NboZzCf1TCAEv0tj5GrfpNICytfFlKe5cxaVa13UdR4Ww";
  bitcoind-mainnet-rpc-pskhmac = "6cd1a9449750a15c9f6d64b96ee089b9$9f5a5378a4ee0785fae6621c8506168af4b9e2211b2e7a1555012f5d6638744c";
  env = {
    bitcoind-mainnet-rpc-psk      = bitcoind-mainnet-rpc-psk;
    bitcoind-mainnet-rpc-pskhmac = bitcoind-mainnet-rpc-pskhmac;
    op-energy-db-psk-mainnet     = op-energy-db-psk-mainnet;
    op-energy-db-salt-mainnet    = op-energy-db-salt-mainnet;
    op-energy-account-token-encryption-key = op-energy-account-token-encryption-key;
    op-energy-internal-service-shared-secret = op-energy-internal-service-shared-secret;
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
