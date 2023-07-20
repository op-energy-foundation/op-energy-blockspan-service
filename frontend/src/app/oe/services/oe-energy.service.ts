import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import {
  TimeStrike,
  SlowFastGuess,
  TimeStrikesHistory,
  SlowFastResult,
  EnergyNbdrStatistics,
  BlockSpan,
  BlockHeader,
  Block,
  SwaggerJson,
  BackendGitHash,
  BlockSpanHeadersNbdr,
  BlockSpanHeaders,
  RegisterResult,
} from '../interfaces/oe-energy.interface';
import { take, switchMap } from 'rxjs/operators';
import { OeStateService } from './state.service';

@Injectable({
  providedIn: 'root',
})
export class OeEnergyApiService {
  private apiBaseUrl: string; // base URL is protocol, hostname, and port
  private apiBasePath: string; // network path is /testnet, etc. or '' for mainnet
  constructor(
    private httpClient: HttpClient,
    private oeEnergyStateService: OeStateService
  ) {
    this.apiBaseUrl =
      document.location.protocol + '//' + document.location.host; // use relative URL by default
    this.apiBasePath = '';
  }

  // adds blocked, locked by time by current user
  // params:
  // - blockHeight - height of the block
  // - nlocktime - time, by which lock is being blocked
  // returns TimeStrike value in case of success or throws error otherwise
  $addTimeStrike(
    blockHeight: number,
    nlocktime: number
  ): Observable<TimeStrike> {
    var accountToken;
    // get account token from the state service
    let subscription = this.oeEnergyStateService.$accountToken.subscribe(
      (newAccountToken) => {
        accountToken = newAccountToken;
      }
    );
    subscription.unsubscribe();

    let params = {
      block_height: blockHeight,
      nlocktime: nlocktime,
      account_token: accountToken,
    };

    return this.httpClient.post<TimeStrike>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/strike/mediantime',
      params,
      {
        observe: 'body',
        responseType: 'json',
      }
    );
  }
  // returns list of available locked blocks or throws error in case of failure
  $listTimeStrikes(): Observable<TimeStrike[]> {
    let params = {};

    return this.httpClient.get<TimeStrike[]>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/strike/mediantime',
      { params }
    );
  }
  // returns list of available locked blocks for a given block height or throws error in case of failure
  $listTimeStrikesByBlockHeight(blockHeight: number): Observable<TimeStrike[]> {
    let params = {
      block_height: blockHeight,
    };

    return this.httpClient.get<TimeStrike[]>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/strike/block/mediantime',
      { params }
    );
  }
  // this function returns an observable with value of type SlowFastGuess, meaning, that the guess had been persisted in the DB
  // params:
  // - guess: "slow" or "fast"
  // - lockedBlockHeight: height of the locked block number
  // - medianSeconds: value of locked block's median time to guess
  $slowFastGuess(
    guess: 'slow' | 'fast',
    timeStrike: TimeStrike
  ): Observable<SlowFastGuess> {
    return this.oeEnergyStateService.$accountToken.pipe(take(1)).pipe(
      switchMap((newAccountToken) => {
        let params = {
          account_token: newAccountToken,
          block_height: timeStrike.blockHeight,
          nlocktime: timeStrike.nLockTime,
          guess: guess,
        };

        return this.httpClient.post<SlowFastGuess>(
          this.apiBaseUrl +
            this.apiBasePath +
            '/api/v1/slowfastguess/mediantime',
          params,
          {
            observe: 'body',
            responseType: 'json',
          }
        );
      })
    );
  }
  // returns list of the guesses for a given timelocked block
  $listSlowFastGuesses(timeStrike: TimeStrike): Observable<SlowFastGuess[]> {
    let params = {
      block_height: timeStrike.blockHeight,
      nlocktime: timeStrike.nLockTime,
    };

    return this.httpClient.get<SlowFastGuess[]>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/slowfastguess/mediantime',
      { params }
    );
  }

  // updates displayable user name for a current user
  // params:
  // - guess: "slow" or "fast"
  // - lockedBlockHeight: height of the locked block number
  // - medianSeconds: value of locked block's median time to guess
  $updateUserDisplayName(displayName: string): Observable<string> {
    return this.oeEnergyStateService.$accountToken.pipe(take(1)).pipe(
      switchMap((newAccountToken) => {
        let params = {
          account_token: newAccountToken,
          display_name: displayName,
        };

        return this.httpClient.post<string>(
          this.apiBaseUrl + this.apiBasePath + '/api/v1/user/displayname',
          params,
          {
            observe: 'body',
            responseType: 'json',
          }
        );
      })
    );
  }

  // returns list of strikes results or throws error in case of failure
  $listTimeStrikesHistory(): Observable<TimeStrikesHistory[]> {
    return this.httpClient.get<TimeStrikesHistory[]>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/strikeshistory/mediantime',
      {}
    );
  }

  // returns list of the slow/fast guess results for a given timelocked block
  $listSlowFastResults(
    timeStrikesHistory: TimeStrikesHistory
  ): Observable<SlowFastResult | null> {
    return this.oeEnergyStateService.$accountToken.pipe(take(1)).pipe(
      switchMap((newAccountToken) => {
        let params = {
          account_token: newAccountToken,
          block_height: timeStrikesHistory.blockHeight,
          nlocktime: timeStrikesHistory.nLockTime,
        };

        return this.httpClient.get<SlowFastResult | null>(
          this.apiBaseUrl +
            this.apiBasePath +
            '/api/v1/slowfastresults/mediantime',
          { params }
        );
      })
    );
  }

  $getBlock(hash: string): Observable<Block> {
    return this.httpClient.get<Block>(
      this.apiBaseUrl + this.apiBasePath + '/api/oe/block/' + hash
    );
  }

  $getBlockByHeight(height: number): Observable<BlockHeader> {
    return this.httpClient.get<BlockHeader>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/oe/blockbyheight/' + height
    );
  }

  $getNbdrStatistics(
    blockHeight: number,
    span: number
  ): Observable<EnergyNbdrStatistics> {
    return this.httpClient.get<EnergyNbdrStatistics>(
      `${this.apiBaseUrl}${this.apiBasePath}/api/v1/statistics/${blockHeight}/${span}`
    );
  }

  $getBlockSpanList(
    startBlockHeight: number,
    span: number,
    numberOfSpan: number
  ): Observable<BlockSpan[]> {
    return this.httpClient.get<BlockSpan[]>(
      `${this.apiBaseUrl}${this.apiBasePath}/api/v1/oe/blockspanlist/${startBlockHeight}/${span}/${numberOfSpan}`
    );
  }

  // returns swagger API json
  $getSwaggerFile(): Observable<SwaggerJson> {
    return this.httpClient.get<SwaggerJson>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/swagger.json',
      {}
    );
  }

  // returns swagger API json
  $getGitHash(): Observable<BackendGitHash> {
    return this.httpClient.get<BackendGitHash>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/oe/git-hash',
      {}
    );
  }

  // returns list of start and end block for given blockspan
  $getBlocksByBlockSpan(
    startBlockHeight: number,
    span: number,
    mNumberOfSpan?: number,
    withNbdr = false,
    withHashrate = false
  ): Observable<BlockHeader[][]> {
    let queryParam = `?withNBDR=${withNbdr}&withHashrate=${withHashrate}`;
    queryParam =
      queryParam +
      (typeof mNumberOfSpan !== 'undefined'
        ? `&numberOfSpan=${mNumberOfSpan}`
        : '');
    return this.httpClient.get<BlockHeader[][]>(
      `${this.apiBaseUrl}${this.apiBasePath}/api/v1/oe/blocksbyblockspan/${startBlockHeight}/${span}${queryParam}`
    );
  }

  // return block with nbdr data
  $getBlocksWithNbdrByBlockSpan(
    startBlockHeight: number,
    span: number,
    mNumberOfSpan?: number
  ): Observable<BlockSpanHeadersNbdr[]> {
    const numberOfSpan =
      typeof mNumberOfSpan !== 'undefined'
        ? `?numberOfSpan=${mNumberOfSpan}`
        : '';
    return this.httpClient.get<BlockSpanHeadersNbdr[]>(
      `${this.apiBaseUrl}${this.apiBasePath}/api/v1/oe/blockswithnbdrbyblockspan/${startBlockHeight}/${span}${numberOfSpan}`
    );
  }

  // return block with nbdr data
  $getBlocksWithHashrateByBlockSpan(
    startBlockHeight: number,
    span: number,
    mNumberOfSpan?: number
  ): Observable<BlockSpanHeaders[]> {
    const numberOfSpan =
      typeof mNumberOfSpan !== 'undefined'
        ? `?numberOfSpan=${mNumberOfSpan}`
        : '';
    return this.httpClient.get<BlockSpanHeaders[]>(
      `${this.apiBaseUrl}${this.apiBasePath}/api/v1/oe/blockswithhashratebyblockspan/${startBlockHeight}/${span}${numberOfSpan}`
    );
  }
}

@Injectable({
  providedIn: 'root',
})
export class OeAccountApiService {
  private apiBaseUrl: string; // base URL is protocol, hostname, and port
  private apiBasePath: string; // network path is /testnet, etc. or '' for mainnet
  constructor(
    private httpClient: HttpClient,
    private oeEnergyStateService: OeStateService
  ) {
    this.apiBaseUrl =
      document.location.protocol + '//' + document.location.host; // use relative URL by default
    this.apiBasePath = '';
  }

  // registers new user. See API specs for reference
  // it is expected that frontend should keep token in the state service and use it for the rest API calls,
  // that require authentication.
  $register(
  ): Observable<RegisterResult> {
    return this.httpClient.post<RegisterResult>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/account/register',
      {}
    );
  }

  // logs in user with given secret. returns account token. See API for reference
  // it is expected that frontend should keep token in the state service and use it for the rest API calls,
  // that require authentication.
  // params:
  // - secret: secret value returned by $register() call
  $login(
    secret: string,
  ): Observable<string> {
    let headers = new HttpHeaders({
      'Content-Type': 'application/json'
    });

    return this.httpClient.post<string>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/account/login',
      JSON.stringify(secret),
      {
        observe: 'body',
        responseType: 'json',
        headers
      }
    );
  }

  // updates displayable user name for a current user. Can fail if there is a user with given display name exist
  // params:
  // - accountToken: token got from register/login
  // - displayName: new display name
  $updateUserDisplayName(
    accountToken: string,
    displayName: string
  ): Observable<string> {
    let params = {
      account_token: accountToken,
      display_name: displayName,
    };

    return this.httpClient.post<string>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/account/displayname',
      params,
      {
        observe: 'body',
        responseType: 'json',
      }
    );
  }

  // returns swagger API json
  $getSwaggerFile(): Observable<SwaggerJson> {
    return this.httpClient.get<SwaggerJson>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/account/swagger.json',
      {}
    );
  }

  // returns swagger API json
  $getGitHash(): Observable<BackendGitHash> {
    return this.httpClient.get<BackendGitHash>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/account/git-hash',
      {}
    );
  }
}

@Injectable({
  providedIn: 'root',
})
export class OeBlocktimeApiService {
  private apiBaseUrl: string; // base URL is protocol, hostname, and port
  private apiBasePath: string; // network path is /testnet, etc. or '' for mainnet
  constructor(
    private httpClient: HttpClient,
    private oeEnergyStateService: OeStateService
  ) {
    this.apiBaseUrl =
      document.location.protocol + '//' + document.location.host; // use relative URL by default
    this.apiBasePath = '';
  }

  // adds blocked, locked by time by current user
  // params:
  // - blockHeight - height of the block
  // - nlocktime - time, by which lock is being blocked
  // returns TimeStrike value in case of success or throws error otherwise
  $addTimeStrike(
    blockHeight: number,
    nlocktime: number
  ): Observable<TimeStrike> {
    var accountToken;
    // get account token from the state service
    let subscription = this.oeEnergyStateService.$accountToken.subscribe(
      (newAccountToken) => {
        accountToken = newAccountToken;
      }
    );
    subscription.unsubscribe();

    let params = {
      block_height: blockHeight,
      nlocktime: nlocktime,
      account_token: accountToken,
    };

    return this.httpClient.post<TimeStrike>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/strike/mediantime',
      params,
      {
        observe: 'body',
        responseType: 'json',
      }
    );
  }
  // returns list of available locked blocks or throws error in case of failure
  $listTimeStrikes(): Observable<TimeStrike[]> {
    let params = {};

    return this.httpClient.get<TimeStrike[]>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/strike/mediantime',
      { params }
    );
  }
  // returns list of available locked blocks for a given block height or throws error in case of failure
  $listTimeStrikesByBlockHeight(blockHeight: number): Observable<TimeStrike[]> {
    let params = {
      block_height: blockHeight,
    };

    return this.httpClient.get<TimeStrike[]>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/strike/block/mediantime',
      { params }
    );
  }
  // this function returns an observable with value of type SlowFastGuess, meaning, that the guess had been persisted in the DB
  // params:
  // - guess: "slow" or "fast"
  // - lockedBlockHeight: height of the locked block number
  // - medianSeconds: value of locked block's median time to guess
  $slowFastGuess(
    guess: 'slow' | 'fast',
    timeStrike: TimeStrike
  ): Observable<SlowFastGuess> {
    return this.oeEnergyStateService.$accountToken.pipe(take(1)).pipe(
      switchMap((newAccountToken) => {
        let params = {
          account_token: newAccountToken,
          block_height: timeStrike.blockHeight,
          nlocktime: timeStrike.nLockTime,
          guess: guess,
        };

        return this.httpClient.post<SlowFastGuess>(
          this.apiBaseUrl +
            this.apiBasePath +
            '/api/v1/slowfastguess/mediantime',
          params,
          {
            observe: 'body',
            responseType: 'json',
          }
        );
      })
    );
  }
  // returns list of the guesses for a given timelocked block
  $listSlowFastGuesses(timeStrike: TimeStrike): Observable<SlowFastGuess[]> {
    let params = {
      block_height: timeStrike.blockHeight,
      nlocktime: timeStrike.nLockTime,
    };

    return this.httpClient.get<SlowFastGuess[]>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/slowfastguess/mediantime',
      { params }
    );
  }

  // updates displayable user name for a current user
  // params:
  // - guess: "slow" or "fast"
  // - lockedBlockHeight: height of the locked block number
  // - medianSeconds: value of locked block's median time to guess
  $updateUserDisplayName(displayName: string): Observable<string> {
    return this.oeEnergyStateService.$accountToken.pipe(take(1)).pipe(
      switchMap((newAccountToken) => {
        let params = {
          account_token: newAccountToken,
          display_name: displayName,
        };

        return this.httpClient.post<string>(
          this.apiBaseUrl + this.apiBasePath + '/api/v1/user/displayname',
          params,
          {
            observe: 'body',
            responseType: 'json',
          }
        );
      })
    );
  }

  // returns list of strikes results or throws error in case of failure
  $listTimeStrikesHistory(): Observable<TimeStrikesHistory[]> {
    return this.httpClient.get<TimeStrikesHistory[]>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/strikeshistory/mediantime',
      {}
    );
  }

  // returns list of the slow/fast guess results for a given timelocked block
  $listSlowFastResults(
    timeStrikesHistory: TimeStrikesHistory
  ): Observable<SlowFastResult | null> {
    return this.oeEnergyStateService.$accountToken.pipe(take(1)).pipe(
      switchMap((newAccountToken) => {
        let params = {
          account_token: newAccountToken,
          block_height: timeStrikesHistory.blockHeight,
          nlocktime: timeStrikesHistory.nLockTime,
        };

        return this.httpClient.get<SlowFastResult | null>(
          this.apiBaseUrl +
            this.apiBasePath +
            '/api/v1/slowfastresults/mediantime',
          { params }
        );
      })
    );
  }

  $getBlock(hash: string): Observable<Block> {
    return this.httpClient.get<Block>(
      this.apiBaseUrl + this.apiBasePath + '/api/oe/block/' + hash
    );
  }

  $getBlockByHeight(height: number): Observable<BlockHeader> {
    return this.httpClient.get<BlockHeader>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/oe/blockbyheight/' + height
    );
  }

  $getNbdrStatistics(
    blockHeight: number,
    span: number
  ): Observable<EnergyNbdrStatistics> {
    return this.httpClient.get<EnergyNbdrStatistics>(
      `${this.apiBaseUrl}${this.apiBasePath}/api/v1/statistics/${blockHeight}/${span}`
    );
  }

  $getBlockSpanList(
    startBlockHeight: number,
    span: number,
    numberOfSpan: number
  ): Observable<BlockSpan[]> {
    return this.httpClient.get<BlockSpan[]>(
      `${this.apiBaseUrl}${this.apiBasePath}/api/v1/oe/blockspanlist/${startBlockHeight}/${span}/${numberOfSpan}`
    );
  }

  // returns swagger API json
  $getSwaggerFile(): Observable<SwaggerJson> {
    return this.httpClient.get<SwaggerJson>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/swagger.json',
      {}
    );
  }

  // returns swagger API json
  $getGitHash(): Observable<BackendGitHash> {
    return this.httpClient.get<BackendGitHash>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/oe/git-hash',
      {}
    );
  }

  // returns list of start and end block for given blockspan
  $getBlocksByBlockSpan(
    startBlockHeight: number,
    span: number,
    numberOfSpan: number,
    withNbdr = false,
    withHashrate = false
  ): Observable<BlockHeader[][]> {
    return this.httpClient.get<BlockHeader[][]>(
      `${this.apiBaseUrl}${this.apiBasePath}/api/v1/oe/blocksbyblockspan/${startBlockHeight}/${span}/${numberOfSpan}?withNBDR=${withNbdr}&withHashrate=${withHashrate}`
    );
  }

  // return block with nbdr data
  $getBlocksWithNbdrByBlockSpan(
    startBlockHeight: number,
    span: number,
    numberOfSpan: number
  ): Observable<BlockSpanHeadersNbdr[]> {
    return this.httpClient.get<BlockSpanHeadersNbdr[]>(
      `${this.apiBaseUrl}${this.apiBasePath}/api/v1/oe/blockswithnbdrbyblockspan/${startBlockHeight}/${span}/${numberOfSpan}`
    );
  }
}
