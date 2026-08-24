{config, ...}:
URL_BASE:
API_HOST:
let
  zones_enabled =
    if config.services ? "op-energy-backend"
      then config.services.op-energy-backend != {}
      else false;
in
{
  # TODO: composeable global config?! limits?!
  locations = {
    "${URL_BASE}api/v2/blockspans/ws" = {
      proxyPass = "${API_HOST}/api/v2/blockspans/ws";
      proxyWebsockets = true;
      extraConfig = if zones_enabled
        then ''
          limit_conn websocket 100;
        ''
        else "";
    };
    "${URL_BASE}api/v2/blockspans" = {
      proxyPass = "${API_HOST}/api/v2/blockspans";
      extraConfig = if zones_enabled
        then ''
          limit_req zone=api burst=10 nodelay;
        ''
        else "";
    };
  };
}

