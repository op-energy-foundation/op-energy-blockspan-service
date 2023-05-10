const PROXY_CONFIG = require('./proxy.conf.local');

PROXY_CONFIG.forEach(entry => {
  entry.target = "http://exchange.op-energy.info";
});

module.exports = PROXY_CONFIG;
