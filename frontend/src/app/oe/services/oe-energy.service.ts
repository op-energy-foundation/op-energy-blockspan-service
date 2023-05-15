import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
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
} from '../interfaces/oe-energy.interface';
import { take, switchMap } from 'rxjs/operators';
import { OeStateService } from './state.service';
import { environment } from '../../../environments/environment';

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
    this.apiBaseUrl = environment.baseUrl; // use relative URL by default
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

}
