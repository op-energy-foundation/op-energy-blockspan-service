export const environment = {
  production: false,
  baseUrl: 'https://exchange.op-energy.info',
  KEEP_BLOCKS_AMOUNT: 11,
  // V2 Blockspan API toggle:
  // - Set to 'true' when backend server supports V2 endpoints (/api/v2/blockspans/*)
  // - Set to 'false' to use V1 endpoints (/api/v1/oe/*)
  // - Currently disabled because dev-exchange.op-energy.info only has V1 endpoints deployed
  useV2BlockspanApi: false
};
