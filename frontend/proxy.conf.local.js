const PROXY_CONFIG = [];

// All APIs go to dev-exchange
PROXY_CONFIG.push({
  context: ['/api/**'],
  target: 'https://dev-exchange.op-energy.info',
  secure: false,
  changeOrigin: true,
  ws: true,
  proxyTimeout: 30000,
  logLevel: 'debug',
});

module.exports = PROXY_CONFIG;