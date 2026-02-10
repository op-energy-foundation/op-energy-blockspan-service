const PROXY_CONFIG = [];

PROXY_CONFIG.push(...[
  {
    context: ['/api/**'],
    target: `http://localhost:8999`,
    secure: false,
    changeOrigin: true,
    proxyTimeout: 30000,
  }
]);

module.exports = PROXY_CONFIG;
