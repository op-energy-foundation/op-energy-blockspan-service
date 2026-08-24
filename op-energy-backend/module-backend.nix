{ GIT_COMMIT_HASH}:
args@{config, pkgs, options, lib, ...}:
let
  op-energy-overlay = (import ../overlay.nix) { GIT_COMMIT_HASH = GIT_COMMIT_HASH; };
  initial_script = cfg:
    pkgs.writeText "initial_script.sql" ''
    do $$
    begin
      if not exists (select * from pg_user where usename = '${cfg.db_user}') then
        CREATE USER ${cfg.db_user} WITH PASSWORD '${cfg.db_psk}';
      end if;
      ALTER USER ${cfg.db_user} WITH PASSWORD '${cfg.db_psk}';
      GRANT ALL PRIVILEGES ON DATABASE ${cfg.db_name} TO ${cfg.db_user};
      ALTER DATABASE ${cfg.db_name} OWNER TO ${cfg.db_user};
    end
    $$
    ;
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
      db_psk = lib.mkOption {
        type = lib.types.str;
        default = null;
        example = "your-secret-from-out-of-git-store";
        description = ''
          This value defines a password for database user, which will be used by op-energy backend instance to access database.
        '';
      };
      config = lib.mkOption {
        type = lib.types.str;
        default = "";
        example = ''
          {
            "DB_PORT": 5432,
            "DB_HOST": "127.0.0.1",
            "DB_USER": "openergy",
            "DB_NAME": "openergy",
            "DB_PASSWORD": "password",
            "SECRET_SALT": "salt",
            "API_HTTP_PORT": 8999,
            "BTC_URL": "http://127.0.0.1:8332",
            "BTC_USER": "op-energy",
            "BTC_PASSWORD": "password1",
            "BTC_POLL_RATE_SECS": 10,
            "SCHEDULER_POLL_RATE_SECS": 10
          }
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
          {
            "DB_PORT": 5432,
            "DB_HOST": "127.0.0.1",
            "DB_USER": "openergy",
            "DB_NAME": "openergy",
            "DB_PASSWORD": "password",
            "SECRET_SALT": "salt",
            "API_HTTP_PORT": 8999,
            "BTC_URL": "http://127.0.0.1:8332",
            "BTC_USER": "op-energy",
            "BTC_PASSWORD": "password1",
            "BTC_POLL_RATE_SECS": 10,
            "SCHEDULER_POLL_RATE_SECS": 10
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
    systemd.services = {
      postgresql-op-energy-users = {
        wantedBy = [ "multi-user.target" ];
        after = [
          "postgresql.service"
        ];
        requires = [
          "postgresql.service"
        ];
        serviceConfig = {
          Type = "simple";
        };
        path = with pkgs; [
          postgresql sudo
        ];
        preStart = lib.foldl' (acc: i: acc + i) '''' ( lib.mapAttrsToList (name: cfg: ''
          # create database if not exist. we can't use services.mysql.ensureDatabase/initialDatase here the latter
          # will not use schema and the former will only affects the very first start of mariadb service, which is not idemponent
          if [ ! "$(sudo -u postgres psql -l -x --csv | grep 'Name,${cfg.db_name}' --count)" == "1" ]; then
            ( echo 'CREATE DATABASE ${cfg.db_name};'
              echo '\c ${cfg.db_name};'
            ) | sudo -u postgres psql
          fi
          cat "${initial_script cfg}" | sudo -u postgres psql
        '') eachInstance);
        script = "exit 0";
      };
    } // ( lib.mapAttrs' (name: cfg: lib.nameValuePair "op-energy-backend-${name}" (
      let
        openergy_config = pkgs.writeText "op-energy-config.json" cfg.config; # this renders config and stores in /nix/store
      in {
        wantedBy = [ "multi-user.target" ];
        after = [
          "network-online.target"
          "postgresql.service"
          "postgresql-op-energy-users.service"
        ];
        requires = [
          "network-online.target"
          "postgresql.service"
          "postgresql-op-energy-users.service"
          ];
        serviceConfig = {
          Type = "simple";
          Restart = "always"; # we want to keep service always running, especially, now development instance is relying on ssh tunnel which can restart as well leading to op-energy restart as well
          StartLimitIntervalSec = 0;
          StartLimitBurst = 0;
        };
        path = with pkgs; [
          pkgs.op-energy-backend
        ];
        script = ''
          set -ex
          sleep ${toString cfg.startup_delay}s
          OPENERGY_BACKEND_CONFIG_FILE="${openergy_config}" op-energy-backend +RTS -c -N -s
        '';
      })) eachInstance);
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
