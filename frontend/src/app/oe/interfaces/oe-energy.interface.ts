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
  stage?: number;
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

export interface WebsocketResponse {
  block?: Block;
  blocks?: Block[];
  action?: string;
  data?: string[];
}
export interface OpEnergyWebsocketResponse extends WebsocketResponse {
  'track-time-strikes'?: 'start' | 'stop';
  'track-time-strike-start'?: TimeStrike;
  'track-time-strike-stop'?: TimeStrike;
  timeStrike?: TimeStrike;
  timeSlowFastGuess?: SlowFastGuess;
  timeSlowFastGuessOutcome?: SlowFastGuessOutcome;
  generatedAccountSecret?: string;
  generatedAccountToken?: string;
  checkedAccountToken?: string;
  declinedAccountSecret?: string;
}


export interface SwaggerJson {
  [key: string]: string;
}
