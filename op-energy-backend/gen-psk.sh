# this script is generating the passwords for op-energy instances. Requires 2
# command-line arguments: output directory and space separated list of bitcoin networks
# Requires:
# - python3 - for running rpcauth.py (from bitcoin)
# - curl / wget - for downloading rpcauth.py
#
# Generated files (per network):
#   bitcoind-<network>-rpc-pskhmac.txt              - bitcoind RPC rpcauth HMAC line
#   OP_ENERGY_BLOCKSPANS_<NETWORK>_BTC_PASSWORD_SECRET  - blockspan service bitcoind RPC password
#   OP_ENERGY_BLOCKSPANS_<NETWORK>_DB_PASSWORD_SECRET    - blockspan service DB password
# Generated files (one per instance, not per network):
#   OP_ENERGY_ACCOUNT_DB_PASSWORD_SECRET            - account service DB password
#   OP_ENERGY_ACCOUNT_SECRET_SALT_SECRET            - account service secret salt
#   OP_ENERGY_ACCOUNT_TOKEN_ENCRYPTION_PRIVATE_KEY_SECRET - account token encryption key (96 bytes, base64)
#   INTERNAL_SERVICE_SHARED_SECRET                  - shared secret between internal services (32 bytes, base64)
#   OP_ENERGY_OFFER_DB_PASSWORD_SECRET            - offer service DB password

set -ex

declare -A db_users=( ["mainnet"]="op-energy" ["testnet"]="top-energy" ["signet"]="sop-energy")
declare -A db_names=( ["mainnet"]="op-energy" ["testnet"]="top-energy" ["signet"]="sop-energy")

OUT_DIR=$1
NETWORKS="$2"

if [ "$OUT_DIR" == "" ] || [ "$NETWORKS" == "" ]; then
  echo "./$0 <OUTPUT_DIRECTORY> \"<networks_list>\""
  echo "where"
  echo "<networks_list> - space separated list of bitcoin networks, like: \"mainnet testnet signet\""
  exit 1
fi

mkdir -p "$OUT_DIR"

function curlOrWget() {
    local backend=curl
    curl --version >/dev/null 2>/dev/null || {
        backend="wget -O -"
        wget --verion > /dev/null 2>/dev/null || {
            echo "ERROR: there is no curl or wget available"
            exit 1
        }
    }
    $backend $@
}

for NETWORK in $NETWORKS; do
    NET=${NETWORK^^}
    # bitcoind RPC password for blockspan service
    PSK=$(dd if=/dev/urandom bs=1 count=10 2>/dev/null | sha256sum | awk '{print $1}')
    printf "%s" "$PSK" > $OUT_DIR/OP_ENERGY_BLOCKSPANS_${NET}_BTC_PASSWORD_SECRET
    HMAC=$(curlOrWget https://raw.githubusercontent.com/bitcoin/bitcoin/master/share/rpcauth/rpcauth.py | python3 - "${db_users[$NETWORK]}" "$PSK" | grep rpcauth | awk 'BEGIN{FS=":"}{print $2}')
    printf "%s" "$HMAC" > $OUT_DIR/bitcoind-$NETWORK-rpc-pskhmac.txt
    # DB password for blockspan service
    PSK=$(dd if=/dev/urandom bs=1 count=10 2>/dev/null | sha256sum | awk '{print $1}')
    printf "%s" "$PSK" > $OUT_DIR/OP_ENERGY_BLOCKSPANS_${NET}_DB_PASSWORD_SECRET
done

# credentials for account service
PSK=$(dd if=/dev/urandom bs=1 count=10 2>/dev/null | sha256sum | awk '{print $1}')
printf "%s" "$PSK" > $OUT_DIR/OP_ENERGY_ACCOUNT_DB_PASSWORD_SECRET
PSK=$(dd if=/dev/urandom bs=1 count=10 2>/dev/null | sha256sum | awk '{print $1}')
printf "%s" "$PSK" > $OUT_DIR/OP_ENERGY_OFFER_DB_PASSWORD_SECRET
SALT=$(dd if=/dev/urandom bs=1 count=10 2>/dev/null | sha256sum | awk '{print $1}')
printf "%s" "$SALT" > $OUT_DIR/OP_ENERGY_ACCOUNT_SECRET_SALT_SECRET
TOKEN_ENCRYPTION_KEY=$(dd if=/dev/urandom bs=1 count=96 2>/dev/null | base64 -w 0)
printf "%s" "$TOKEN_ENCRYPTION_KEY" > $OUT_DIR/OP_ENERGY_ACCOUNT_TOKEN_ENCRYPTION_PRIVATE_KEY_SECRET
INTERNAL_SERVICE_SHARED_SECRET=$(dd if=/dev/urandom bs=1 count=32 2>/dev/null | base64 -w 0)
printf "%s" "$INTERNAL_SERVICE_SHARED_SECRET" > $OUT_DIR/INTERNAL_SERVICE_SHARED_SECRET
PSK=$(dd if=/dev/urandom bs=1 count=10 2>/dev/null | sha256sum | awk '{print $1}')
printf "%s" "$PSK" > $OUT_DIR/LITD_UI_PASSWORD_SECRET
