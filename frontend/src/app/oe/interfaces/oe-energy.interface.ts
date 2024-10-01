export interface Block {
  height: number;
  hash?: string;
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
  strikeMediantime: number;
  creationTime: number;
  elapsedTime?: number;
}

export interface SlowFastGuess {
  guess: 'slow' | 'fast';
  blockHeight: number;
  strikeMediantime: number;
  creationTime: number;
  userName: string;
  userId: number;
}

export interface SlowFastGuessOutcome {
  outcome: 'slow' | 'fast';
  blockHeight: number;
  strikeMediantime: number;
}

export interface TimeStrikesHistory {
  owner: string;
  blockHeight: number;
  strikeMediantime: number;
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
  strikeMediantime: number;
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
export interface OeEnergyWebsocketResponse extends WebsocketResponse {
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

export interface BackendGitHash {
  gitCommitHash: string;
}

export interface BlockSpanHeadersNbdrHashRate {
  startBlock: BlockHeader;
  endBlock: BlockHeader;
  nbdr: number;
  hashrate: string;
}

export interface BlockSpanHeadersNbdr {
  startBlock: BlockHeader;
  endBlock: BlockHeader;
  nbdr: number;
}

export interface RegisterResult {
  accountSecret: string;
  accountToken: string;
  personUUID: string;
}

export interface LoginResult {
  accountToken: string;
  personUUID: string;
}

export interface BlockSpanHeaders
  extends BlockSpanHeadersNbdr,
    BlockSpanHeadersNbdrHashRate {}

export interface BlockTimeStrike {
  block : number;
  strikeMediantime : number;
  observedResult? : 'slow' | 'fast';
  observedBlockMediantime? : number;
  observedBlockHash? : string;
  creationTime : number;
}

export interface BlockTimeStrikeGuessPublic {
  person : string;
  strike : BlockTimeStrike;
  creationTime : number;
  guess : 'slow' | 'fast';
}

export interface StrikeDetails {
  block: number;
  creationTime: number;
  futureStrikeCreationTime: number;
  strikeMediantime: number;
  observedBlockHash: string;
  observedBlockMediantime: number;
  observedResult: string;
}

export interface TableColumn {
  displayName: string;
  columnKey: string;
  isSrNo?: boolean;
  customSrNoHeader?: string;
  defaultValue?: string;
}

export interface BlockTimeStrikePublic {
  guessesCount : number;
  strike : BlockTimeStrike;
}

export type PaginationResponse<T> = {
  nextPage: number;
  count: number;
  results: T[];
};

export interface StrikesFilter {
  strikeBlockHeightGTE?: number;
  strikeBlockHeightLTE?: number;
  strikeMediantimeGTE?: number;
  strikeMediantimeLTE?: number;
  sort?: string;
  class?: string;
  linesPerPage?: number;
}
