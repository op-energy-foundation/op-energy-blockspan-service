{ GIT_COMMIT_HASH}:
args@{config, pkgs, options, lib, ...}:
let
  op-energy-overlay = (import ../overlay.nix) { GIT_COMMIT_HASH = GIT_COMMIT_HASH; };
  initial_script = cfg:
    pkgs.writeText "initial_script.sql" ''
    do $$
    begin
      if not exists (select * from pg_user where usename = '${cfg.db_user}') then
        CREATE USER ${cfg.db_user} WITH PASSWORD 'DB_PASSWORD_SECRET';
      end if;
      ALTER USER ${cfg.db_user} WITH PASSWORD 'DB_PASSWORD_SECRET';
      GRANT ALL PRIVILEGES ON DATABASE ${cfg.db_name} TO ${cfg.db_user};
      ALTER DATABASE ${cfg.db_name} OWNER TO ${cfg.db_user};
    end
    $$
    ;
  '';
  inject_credentials = cfg: file: pkgs.writeScriptBin "inject_credentials" ''
    cat >> ${file} <<EOF
      "DB_PASSWORD": "$(cat $CREDENTIALS_DIRECTORY/DB_PASSWORD_SECRET)",
      "BTC_PASSWORD": "$(cat $CREDENTIALS_DIRECTORY/BTC_PASSWORD)"
    }
    EOF
    '';

  eachInstance = config.services.op-energy-backend;
  instanceOpts = args: {
    options = {
      startup_delay = lib.mkOption {
        type = lib.types.int;
        default = 30;
        example = 30;
        description = "let service wait for dependent service to settle (primary bitcoind preps). Usually, you don't want to change it but CI tests can fail if service will start too early and crash due to bitcoind being not ready to serve";
      };
      db_name = lib.mkOption {
        default = null;
        type = lib.types.str;
        example = "mempool";
        description = "Database name of the instance";
      };
      account_db_name = lib.mkOption {
        default = null;
        type = lib.types.str;
        example = "";
        description = "Deprecated";
      };
      db_user = lib.mkOption {
        default = null;
        type = lib.types.str;
        example = "mempool";
        description = "Username to access instance's database";
      };
      credentials_locations = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        default = {
          BTC_PASSWORD_SECRET = "/etc/nixos/private/OP_ENERGY_BLOCKSPANS_MAINNET_BTC_PASSWORD_SECRET";
          DB_PASSWORD_SECRET =  "/etc/nixos/private/OP_ENERGY_BLOCKSPANS_MAINNET_DB_PASSWORD_SECRET";
        };
        description = ''
          A set of credentials (by it's name) and file path containing it.
          File path is expected to be only readable by the root user.
          In the usage example, content of the BTC_PASSWORD_SECRET and
          DB_PASSWORD_SECRET files will be appended to the service's config file
          '';
        example = {
          BTC_PASSWORD_SECRET = "/etc/nixos/private/BTC_PASSWORD_SECRET";
          DB_PASSWORD_SECRET =  "/etc/nixos/private/DB_PASSWORD_SECRET";
        };
      };
      config = lib.mkOption {
        type = lib.types.str;
        default = "";
        example = ''
            "DB_PORT": 5432,
            "DB_HOST": "127.0.0.1",
            "DB_USER": "openergy",
            "DB_NAME": "openergy",
            "API_HTTP_PORT": 8999,
            "BTC_URL": "http://127.0.0.1:8332",
            "BTC_USER": "op-energy",
            "BTC_POLL_RATE_SECS": 10,
            "SCHEDULER_POLL_RATE_SECS": 10,
        '';
      };
    };
  };
in
{
  options.services.op-energy-backend = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule instanceOpts);
    default = {};
    description = "One or more op-energy-backends";
    example = {
      mainnet = {
        config = ''
            "DB_PORT": 5432,
            "DB_HOST": "127.0.0.1",
            "DB_USER": "openergy",
            "DB_NAME": "openergy",
            "API_HTTP_PORT": 8999,
            "BTC_URL": "http://127.0.0.1:8332",
            "BTC_USER": "op-energy",
            "BTC_POLL_RATE_SECS": 10,
            "SCHEDULER_POLL_RATE_SECS": 10,
        '';
      };
    };
  };

  config = lib.mkIf (eachInstance != {}) {
    nixpkgs.overlays = [
      op-energy-overlay # add op-energy-backend into context
    ];
    environment.systemPackages = [ pkgs.op-energy-backend ];
    # enable postgresql and declare op-energy DB
    services.postgresql = {
      enable = true;
      ensureDatabases = (lib.mapAttrsToList (name: cfg:
        "${cfg.db_name}"
      ) eachInstance
      );
      ensureUsers = ( lib.mapAttrsToList (name: cfg:
        { name = "${cfg.db_user}"; }
      ) eachInstance
      );
    };
    systemd.services =
      ( lib.mapAttrs' (name: cfg: lib.nameValuePair "postgresql-op-energy-users-${name}" {
        wantedBy = [ "multi-user.target" ];
        after = [
          "postgresql.service"
        ];
        requires = [
          "postgresql.service"
        ];
        serviceConfig = {
          Type = "simple";
          LoadCredential =
            # TODO: function: key:dir:file -> "key:dir/file"
            [ "DB_PASSWORD_SECRET:${cfg.credentials_locations.DB_PASSWORD_SECRET}"
            ];
          User = "postgres";
          Group = "postgres";
        };
        path = with pkgs; [
          gnused postgresql
        ];
        preStart = let
          iteration = pkgs.writeScriptBin "iteration" ''
          # create database if not exist. we can't use services.mysql.ensureDatabase/initialDatase here the latter
          # will not use schema and the former will only affects the very first start of mariadb service, which is not idemponent
          if [ ! "$( psql -l -x --csv | grep 'Name,${cfg.db_name}' --count)" == "1" ]; then
            ( echo 'CREATE DATABASE ${cfg.db_name};'
              echo '\c ${cfg.db_name};'
            ) | psql
          fi
          cat "${initial_script cfg}" \
            | sed "s|DB_PASSWORD_SECRET|$(cat $CREDENTIALS_DIRECTORY/DB_PASSWORD_SECRET)|g" \
            | psql 2>/dev/null
        '';
        in ''
          COUNT=0
          MAX_COUNT=10
          while [ "$COUNT" -lt "$MAX_COUNT" ]; do
            ${iteration}/bin/iteration && exit 0 || {
              sleep 1s
              COUNT=$(( $COUNT + 1 ))
            }
          done
          echo "was not able to update DB user and passwords"
          exit 1
        '';
        script = "exit 0";
      }
      ) eachInstance
      ) // ( lib.mapAttrs' (name: cfg: lib.nameValuePair "op-energy-backend-${name}" (
      let
        openergy_config = pkgs.writeText "op-energy-config.json" ''
        {
          ${cfg.config}
        ''; # this renders config and stores in /nix/store
      in {
        wantedBy = [ "multi-user.target" ];
        after = [
          "network-online.target"
          "postgresql.service"
          "postgresql-op-energy-users-${name}.service"
        ];
        requires = [
          "network-online.target"
          "postgresql.service"
          "postgresql-op-energy-users-${name}.service"
          ];
        serviceConfig = {
          Type = "simple";
          Restart = "always"; # we want to keep service always running, especially, now development instance is relying on ssh tunnel which can restart as well leading to op-energy restart as well
          StartLimitIntervalSec = 5;
          StartLimitBurst = 0;
          LoadCredential =
            # TODO: function: key:dir:file -> "key:dir/file"
            [ "BTC_PASSWORD_SECRET:${cfg.credentials_locations.BTC_PASSWORD_SECRET}"
              "DB_PASSWORD_SECRET:${cfg.credentials_locations.DB_PASSWORD_SECRET}"
            ];
          User =  "blockspans-${name}";
          Group = "blockspans-${name}";
        };


        path = with pkgs; [
          pkgs.op-energy-backend
        ];
        script = ''
          set -ex
          mkdir -p ~/.blockspan-service || true
          rm -f ~/.blockspan-service/config.json || true
          cp ${openergy_config} ~/.blockspan-service/config.json
          chmod u+w ~/.blockspan-service/config.json
          chmod og-rwx ~/.blockspan-service/config.json
          ${inject_credentials cfg "~/.blockspan-service/config.json"}/bin/inject_credentials
          sleep ${toString cfg.startup_delay}s
          OPENERGY_BACKEND_CONFIG_FILE=~/.blockspan-service/config.json \
            op-energy-backend +RTS -c -N -s
        '';
      })) eachInstance);
    users.users = lib.mapAttrs'
      ( name: cfg: lib.nameValuePair "blockspans-${name}"
        {
          isNormalUser = true;
          group = "blockspans-${name}";
          createHome = true;
        }
      )
      eachInstance;
    users.groups = lib.mapAttrs'
      (name: cfg: lib.nameValuePair "blockspans-${name}"
        {
        }
      )
      eachInstance;
    services.nginx = {
      enable = true;
      appendConfig = lib.mkDefault ''
        worker_processes auto;
        worker_rlimit_nofile 100000;
      '';
      eventsConfig = lib.mkDefault ''
        worker_connections 9000;
        multi_accept on;
      '';
      serverTokens = lib.mkDefault false;
      clientMaxBodySize = lib.mkDefault "10m";
      commonHttpConfig = lib.mkDefault ''
        sendfile on;
        tcp_nopush on;
        tcp_nodelay on;

        server_name_in_redirect off;



        # reset timed out connections freeing ram
        reset_timedout_connection on;
        # maximum time between packets the client can pause when sending nginx any data
        client_body_timeout 10s;
        # maximum time the client has to send the entire header to nginx
        client_header_timeout 10s;
        # timeout which a single keep-alive client connection will stay open
        keepalive_timeout 69s;
        # maximum time between packets nginx is allowed to pause when sending the client data
        send_timeout 69s;

        # number of requests per connection, does not affect SPDY
        keepalive_requests 1337;

        # enable gzip compression
        gzip on;
        gzip_vary on;
        gzip_comp_level 6;
        gzip_min_length 1000;
        gzip_proxied expired no-cache no-store private auth;
        # text/html is always compressed by gzip module
        gzip_types application/javascript application/json application/ld+json application/manifest+json application/x-font-ttf application/x-web-app-manifest+json application/xhtml+xml application/xml font/opentype image/bmp image/svg+xml image/x-icon text/cache-manifest text/css text/plain text/vcard;

        # limit request body size

        # proxy cache
        proxy_cache off;
        proxy_cache_path /var/cache/nginx keys_zone=cache:20m levels=1:2 inactive=600s max_size=500m;

        # exempt localhost from rate limit
        geo $limited_ip {
                default         1;
                127.0.0.1       0;
        }
        map $limited_ip $limited_ip_key {
                1 $binary_remote_addr;
                0 \'\';
        }

        # rate limit requests
        limit_req_zone $limited_ip_key zone=api:5m rate=200r/m;
        limit_req_status 429;

        # rate limit connections
        limit_conn_zone $limited_ip_key zone=websocket:10m;
        limit_conn_status 429;

        map $http_accept_language $header_lang {
                default en-US;
                ~*^en-US en-US;
                ~*^en en-US;
        }

        map $cookie_lang $lang {
                default $header_lang;
                ~*^en-US en-US;
                ~*^en en-US;
        }
      '';
    };
  };
}
