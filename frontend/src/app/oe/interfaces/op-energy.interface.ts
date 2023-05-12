export interface Block {
  height: number;
  version: number;
  current_block_hash: string;
  previous_block_hash: string;
  merkle_root: string;
  timestamp: number;
  difficulty: number;
  nonce: number;
  reward: number;
  chainwork: string;
  mediantime: number;
}

export interface TimeStrike {
  blockHeight: number;
  nLockTime: number;
  creationTime: number;
  elapsedTime?: number;
}

export interface SlowFastGuess {
  guess: 'slow' | 'fast';
  blockHeight: number;
  nLockTime: number;
  creationTime: number;
  userName: string;
  userId: number;
}

export interface SlowFastGuessOutcome {
  outcome: 'slow' | 'fast';
  blockHeight: number;
  nLockTime: number;
}

export interface TimeStrikesHistory {
  owner: string;
  blockHeight: number;
  nLockTime: number;
  mediantime: number;
  creationTime: number;
  archiveTime: number;
  wrongResults: number;
  rightResults: number;
}

export interface SlowFastResult {
  guess: 'slow' | 'fast';
  result: 'wrong' | 'right';
  blockHeight: number;
  nLockTime: number;
  creationTime: number;
}

export interface NavigationObject {
  state: {
    data: {
      block: Block;
      blockHeight: number;
    };
  };
}

export interface EnergyNbdrStatistics {
  nbdr: {
    avg: number;
    stddev: number;
  };
}

export interface BlockSpan {
  startBlockHeight: number;
  endBlockHeight: number;
}

export interface BlockHeader {
  height: number;
  version: number;
  current_block_hash: string;
  previous_block_hash: string;
  merkle_root: string;
  timestamp: number;
  difficulty: number;
  nonce: number;
  reward: number;
  chainwork: string;
  mediantime: number;
}
