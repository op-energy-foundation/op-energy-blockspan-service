const PROXY_CONFIG = [];

PROXY_CONFIG.push(...[
  {
    context: ['/api/v2/blockspans/ws'],
    target: `https://dev-exchange.op.energy`,
    secure: false,
    ws: true,
    changeOrigin: true,
    proxyTimeout: 30000,
  },
  {
    context: ['/api/**'],
    target: `https://dev-exchange.op.energy`,
    secure: false,
    changeOrigin: true,
    proxyTimeout: 30000,
  }
]);

module.exports = PROXY_CONFIG;
