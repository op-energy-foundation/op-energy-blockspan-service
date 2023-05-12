const PROXY_CONFIG = [];

PROXY_CONFIG.push(...[
  {
    context: ['/api/**'],
    target: `http://exchange.op-energy.info`,
    secure: false,
    changeOrigin: true,
    proxyTimeout: 30000,
  }
]);

module.exports = PROXY_CONFIG;
