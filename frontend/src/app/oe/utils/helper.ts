import { Router } from '@angular/router';
import { Block, BlockHeader } from '../interfaces/oe-energy.interface';

export function navigator(router: Router, link: string): void {
  // Check if the link contains query parameters
  const [path, queryString] = link.split('?');
  const queryParams = {};

  if (queryString) {
    queryString.split('&').forEach((param) => {
      const [key, value] = param.split('=');
      queryParams[key] = value;
    });
  }

  if (Object.keys(queryParams).length > 0) {
    router.navigate([path], { queryParams });
  } else {
    router.navigate([path]);
  }
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

export const getBlockSpanByHeight = (
  tipBlock: number,
  noOfBlock: number,
  span = 14
): { fromBlock: number; toBlock: number } => {
  const fromBlock = tipBlock - span * noOfBlock;
  return {
    fromBlock,
    toBlock: fromBlock + span,
  };
};

export const getEmptyBlockHeader = (height: number): BlockHeader => {
  return {
    height: height,
    version: 0,
    merkle_root: '',
    current_block_hash: '',
    previous_block_hash: '',
    timestamp: 0,
    difficulty: 0,
    nonce: 0,
    reward: 0,
    chainwork: '',
    mediantime: 0,
  };
};

export const getHexValue = (hexString: string): number => {

  // Use parseInt to convert hex string to decimal
  const decimal = parseInt(hexString, 16);

  // Check if the conversion was successful, return 0 if invalid hex string
  if (isNaN(decimal)) {
    return 0;
  }
  return decimal;
};

export const toScientificNotation = (
  decimal: number,
  decimalPlaces: number = 2
): string => decimal.toExponential(decimalPlaces);

export const getNextDifficultyAdjustment = (
  epochBlock: Block
): { startBlock: number; endBlock: number; strikeTime: number } => {
  const currentEpochStartBlock = epochBlock;
  const currentEpochEndHeight = currentEpochStartBlock.height + 2016;
  const expectedStrikeTime = currentEpochStartBlock.mediantime + 2016 * 600;

  return {
    startBlock: currentEpochStartBlock.height,
    endBlock: currentEpochEndHeight,
    strikeTime: expectedStrikeTime,
  };
};

// Method to calculate the time difference in HH:MM format
export const calculateTimeDifference = (
  fromTimestamp: number,
  toTimestamp: number
): string => {
  if (fromTimestamp === 0 || toTimestamp === 0) return '?';

  const differenceInSeconds = toTimestamp - fromTimestamp;

  const hours = Math.floor(differenceInSeconds / 3600); // 3600 seconds in an hour
  const minutes = Math.floor((differenceInSeconds % 3600) / 60); // Get remaining minutes

  // Return formatted time difference in HH:MM
  return `${String(hours).padStart(2, '0')}:${String(minutes).padStart(
    2,
    '0'
  )}`;
};

export const convertToUTC = (unixTimestamp: number): string => {
  if (unixTimestamp === 0) {
    return '?';
  }

  const date = new Date(unixTimestamp * 1000); // Convert seconds to milliseconds

  const year = date.getUTCFullYear();
  const month = String(date.getUTCMonth() + 1).padStart(2, '0'); // Months are 0-based, so add 1
  const day = String(date.getUTCDate()).padStart(2, '0');
  const hours = String(date.getUTCHours()).padStart(2, '0');
  const minutes = String(date.getUTCMinutes()).padStart(2, '0');
  const seconds = String(date.getUTCSeconds()).padStart(2, '0');

  return `${year}-${month}-${day} ${hours}:${minutes}:${seconds}`;
};
