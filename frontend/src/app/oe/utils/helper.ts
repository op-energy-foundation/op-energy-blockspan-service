import { Router } from '@angular/router';

export function navigator(router: Router, link: string): void {
  router.navigate([link]);
}

export function toHHMMSS(secs: number): string {
  if (!(secs > 0)) {
    return '??:??:??';
  }

  const hours = Math.floor(secs / 3600);
  const minutes = Math.floor((secs - hours * 3600) / 60);
  const seconds = secs - hours * 3600 - minutes * 60;

  const strHours = hours < 10 ? `0${hours}` : hours.toString();
  const strMinutes = minutes < 10 ? `0${minutes}` : minutes.toString();
  const strSeconds = seconds < 10 ? `0${seconds}` : seconds.toString();

  return `${strHours}:${strMinutes}:${strSeconds}`;
}

export const downloadChart = (href: string, name: string): void => {
  const a = document.createElement('a');
  a.download = name;
  a.href = href;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
};

export const convertHashrate = (
  hashrate: number,
  decimalPoints = 3
): string => {
  if (hashrate >= 1e24) {
    // Convert to YH/s (yottahashes per second)
    return (hashrate / 1e24).toFixed(decimalPoints) + ' Y';
  } else if (hashrate >= 1e21) {
    // Convert to ZH/s (zettahashes per second)
    return (hashrate / 1e21).toFixed(decimalPoints) + ' Z';
  } else if (hashrate >= 1e18) {
    // Convert to EH/s (exahashes per second)
    return (hashrate / 1e18).toFixed(decimalPoints) + ' E';
  } else if (hashrate >= 1e15) {
    // Convert to PH/s (petahashes per second)
    return (hashrate / 1e15).toFixed(decimalPoints) + ' P';
  } else if (hashrate >= 1e12) {
    // Convert to TH/s (terahashes per second)
    return (hashrate / 1e12).toFixed(decimalPoints) + ' T';
  } else if (hashrate >= 1e9) {
    // Convert to GH/s (gigahashes per second)
    return (hashrate / 1e9).toFixed(decimalPoints) + ' G';
  } else if (hashrate >= 1e6) {
    // Convert to MH/s (megahashes per second)
    return (hashrate / 1e6).toFixed(decimalPoints) + ' M';
  } else if (hashrate >= 1e3) {
    // Convert to KH/s (kilohashes per second)
    return (hashrate / 1e3).toFixed(decimalPoints) + ' K';
  } else {
    // Keep the original value if it's less than 1 KH/s
    return hashrate.toFixed(decimalPoints);
  }
};
