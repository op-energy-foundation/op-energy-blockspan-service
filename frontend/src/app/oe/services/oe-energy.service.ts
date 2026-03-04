import { Injectable } from '@angular/core';
import { BehaviorSubject, Observable, throwError, combineLatest, of } from 'rxjs';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { environment } from '../../../environments/environment';
import {
  TimeStrike,
  BlockHeader,
  SwaggerJson,
  BackendGitHash,
  BlockSpanHeaders,
  RegisterResult,
  BlockTimeStrikePublic,
  BlockTimeStrikeGuessPublic,
  BlockSpanTimeStrike,
  BlockSpanTimeStrikeGuessesSummary,
  PaginationResponse,
  LoginResult,
  EitherBlockSpansResponse,
} from '../interfaces/oe-energy.interface';
import { tap, shareReplay, catchError, map } from 'rxjs/operators';
import { OeStateService } from './state.service';
import { CookieService } from 'ngx-cookie-service';
import { APP_CONFIGURATION } from '../types/constant';
import { getEmptyBlockHeader } from '../utils/helper';

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
  // - strikeMediantime - time, by which lock is being blocked
  // returns TimeStrike value in case of success or throws error otherwise
  $addTimeStrike(
    blockHeight: number,
    strikeMediantime: number
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
      strike_mediantime: strikeMediantime,
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

  $getBlockByHeight(height: number): Observable<BlockHeader> {
    const endpoint = environment.useV2BlockspanApi
      ? `${this.apiBaseUrl}${this.apiBasePath}/api/v2/blockspans/blockbyheight/${height}`
      : `${this.apiBaseUrl}${this.apiBasePath}/api/v1/oe/blockbyheight/${height}`;
    return this.httpClient.get<BlockHeader>(endpoint);
  }

  $getBlocksByHeights(heights: number[]): Observable<BlockHeader[]> {
    if (environment.useV2BlockspanApi) {
      const startHeight = heights[0];
      const endHeight = heights[heights.length - 1];
      const spanSize = endHeight - startHeight;
      const tipHeight = this.oeEnergyStateService.latestReceivedBlockHeight;

      if (endHeight > tipHeight && tipHeight > 0) {
        return this.httpClient.get<{ startBlock: BlockHeader; endBlock: BlockHeader }>(
          `${this.apiBaseUrl}${this.apiBasePath}/api/v2/blockspans/blockspan/${startHeight}?spanSize=${spanSize}`
        ).pipe(
          map(response => [response.endBlock, getEmptyBlockHeader(endHeight)]),
          catchError(() => of(heights.map(h => getEmptyBlockHeader(h))))
        );
      }

      return this.httpClient.get<{ startBlock: BlockHeader; endBlock: BlockHeader }>(
        `${this.apiBaseUrl}${this.apiBasePath}/api/v2/blockspans/blockspan/${endHeight}?spanSize=${spanSize}`
      ).pipe(
        map(response => [response.startBlock, response.endBlock]),
        catchError(() => of(heights.map(h => getEmptyBlockHeader(h))))
      );
    } else {
      return combineLatest(
        heights.map(h => this.$getBlockByHeight(h))
      ).pipe(
        catchError(() => of(heights.map(h => getEmptyBlockHeader(h))))
      );
    }
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
    const endpoint = environment.useV2BlockspanApi
      ? `${this.apiBaseUrl}${this.apiBasePath}/api/v2/blockspans/git-hash`
      : `${this.apiBaseUrl}${this.apiBasePath}/api/v1/oe/git-hash`;
    return this.httpClient.get<BackendGitHash>(endpoint);
  }

  // returns list of start and end block for given blockspan
  $getBlocksByBlockSpan(
    startBlockHeight: number,
    span: number,
    mNumberOfSpan?: number,
    withNbdr = false,
    withHashrate = false
  ): Observable<BlockSpanHeaders[]> {
    if (environment.useV2BlockspanApi) {
      // V2 API returns Either [BlockSpanSummary] [BlockSpanHeaders] as {Left: [...]} or {Right: [...]}
      // With withHeaders=true, we get {Right: BlockSpanHeaders[]}
      let queryParam = typeof mNumberOfSpan !== 'undefined'
        ? `?numberOfSpans=${mNumberOfSpan}&withHeaders=true`
        : '?withHeaders=true';
      return this.httpClient.get<EitherBlockSpansResponse>(
        `${this.apiBaseUrl}${this.apiBasePath}/api/v2/blockspans/blockspans/${startBlockHeight}/${span}${queryParam}`
      ).pipe(
        map((response: EitherBlockSpansResponse) => response.Right || [])
      );
    }
    // V1 fallback
    let queryParam = `?withNBDR=${withNbdr}&withHashrate=${withHashrate}`;
    queryParam =
      queryParam +
      (typeof mNumberOfSpan !== 'undefined'
        ? `&numberOfSpan=${mNumberOfSpan}`
        : '');
    return this.httpClient.get<BlockSpanHeaders[]>(
      `${this.apiBaseUrl}${this.apiBasePath}/api/v1/oe/blocksbyblockspan/${startBlockHeight}/${span}${queryParam}`
    );
  }

}

@Injectable({
  providedIn: 'root',
})
export class OeAccountApiService {
  private apiBaseUrl: string; // base URL is protocol, hostname, and port
  private apiBasePath: string; // network path is /testnet, etc. or '' for mainnet
  private registrationInProgress: BehaviorSubject<boolean> =
    new BehaviorSubject<boolean>(false);
  private registrationObservable: Observable<any> | null = null;
  constructor(
    private httpClient: HttpClient,
    private cookieService: CookieService
  ) {
    this.apiBaseUrl =
      document.location.protocol + '//' + document.location.host; // use relative URL by default
    this.apiBasePath = '';
  }

  // registers new user. See API specs for reference
  // it is expected that frontend should keep token in the state service and use it for the rest API calls,
  // that require authentication.
  $register(): Observable<RegisterResult> {
    if (!this.registrationObservable) {
      this.registrationInProgress.next(true);
      this.registrationObservable = this.httpClient
        .post<RegisterResult>(
          this.apiBaseUrl + this.apiBasePath + '/api/v1/account/register',
          {}
        )
        .pipe(
          tap((data) => {
            this.$saveToken(data.accountToken);
            this.$saveAccountUUID(data.personUUID);
            this.registrationInProgress.next(false);
          }),
          shareReplay(1),
          catchError((error) => {
            this.registrationInProgress.next(false);
            this.registrationObservable = null; // Reset for future attempts
            return throwError(() => new Error(`Registration failed: ${error}`));
          })
        );
    }
    return this.registrationObservable;
  }

  // logs in user with given secret. returns account token. See API for reference
  // it is expected that frontend should keep token in the state service and use it for the rest API calls,
  // that require authentication.
  // params:
  // - secret: secret value returned by $register() call
  $login(secret: string): Observable<LoginResult> {
    let headers = new HttpHeaders({
      'Content-Type': 'application/json',
    });

    return this.httpClient.post<LoginResult>(
      this.apiBaseUrl + this.apiBasePath + '/api/v2/account/login',
      JSON.stringify(secret),
      {
        observe: 'body',
        responseType: 'json',
        headers,
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
  $cleanToken(): void {
    this.cookieService.delete('accountToken');
  }

  $saveToken(token: string): void {
    this.cookieService.set('accountToken', token, undefined, '/');
  }

  $saveAccountUUID(accountUUID: string): void {
    this.cookieService.set('accountUUID', accountUUID , undefined, '/');
  }

  $tokenExists(): boolean {
    return this.cookieService.check('accountToken');
  }

  $getAccountToken(): string {
    return this.cookieService.get('accountToken');
  }

  $getAccountUUID(): string {
    return this.cookieService.get('accountUUID');
  }
}

@Injectable({
  providedIn: 'root',
})
export class OeBlocktimeApiService {
  private apiBaseUrl: string; // base URL is protocol, hostname, and port
  private apiBasePath: string; // network path is /testnet, etc. or '' for mainnet
  private defaultSpanSize = APP_CONFIGURATION.SPAN_SIZE;
  constructor(
    private httpClient: HttpClient,
    private oeEnergyStateService: OeStateService
  ) {
    this.apiBaseUrl =
      document.location.protocol + '//' + document.location.host; // use relative URL by default
    this.apiBasePath = '';
  }

  private resolveSpanSize(spanSize?: number): number {
    return spanSize ?? this.defaultSpanSize;
  }

  // returns swagger API json
  $getSwaggerFile(): Observable<SwaggerJson> {
    return this.httpClient.get<SwaggerJson>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/blocktime/swagger.json',
      {}
    );
  }

  // returns swagger API json
  $getGitHash(): Observable<BackendGitHash> {
    return this.httpClient.get<BackendGitHash>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/blocktime/git-hash',
      {}
    );
  }

  $createStrike(
    accountToken: string,
    blockHeight: number,
    strikeMediantime: number,
  ): Observable<void> {
    return this.httpClient.post<void>(
      this.apiBaseUrl +
        this.apiBasePath +
        `/api/v1/blocktime/strike/${blockHeight}/${strikeMediantime}`,
      {
        headers: { 'Content-Type': 'application/json',
                   'AccountToken': accountToken,
                 }
      }
    );
  }

  $createStrikeGuess(
    accountToken: string,
    blockHeight: number,
    strikeMediantime: number,
    guess: 'slow' | 'fast',
    spanSize?: number
  ): Observable<BlockTimeStrikeGuessPublic> {
    if (environment.useV2StrikesApi) {
      const resolvedSpanSize = this.resolveSpanSize(spanSize);
      const url = `${this.apiBaseUrl}${this.apiBasePath}/api/v2/strikes/blockrate/strike/${blockHeight}/${strikeMediantime}/guess/${guess}?spanSize=${resolvedSpanSize}`;
      return this.httpClient.post<BlockTimeStrikeGuessPublic>(url, null, {
        headers: { 'Content-Type': 'application/json' }
      });
    }
    return this.httpClient.post<BlockTimeStrikeGuessPublic>(
      this.apiBaseUrl +
        this.apiBasePath +
        `/api/v1/blocktime/strike/guess/${blockHeight}/${strikeMediantime}/${guess}`,
      {
        headers: { 'Content-Type': 'application/json',
                   'AccountToken': accountToken,
                 }
      }
    );
  }

  $strikesWithFilter(
    filter: any | {},
    pageNo = 0,
    spanSize?: number
  ): Observable<PaginationResponse<BlockTimeStrikePublic>> {
    if (environment.useV2StrikesApi) {
      const resolvedSpanSize = this.resolveSpanSize(spanSize);
      const url = `${this.apiBaseUrl}${this.apiBasePath}/api/v2/strikes/blockrate/strikes?page=${pageNo}&filter=${encodeURI(JSON.stringify(filter))}&spanSize=${resolvedSpanSize}`;
      return this.httpClient.get<PaginationResponse<BlockSpanTimeStrike>>(url, {
        headers: { 'Content-Type': 'application/json' },
      }).pipe(
        map(response => ({
          nextPage: response.nextPage,
          count: response.count,
          results: response.results.map(strike => ({
            guessesCount: strike.guessesCount,
            strike: {
              block: strike.block,
              strikeMediantime: strike.mediantime,
              creationTime: strike.creationTime,
              observedResult: strike.observedResult,
              observedBlockMediantime: strike.observedBlockMediantime,
              observedBlockHash: strike.observedBlockHash,
              observedBlockHeight: strike.observedBlockHeight,
            }
          }))
        }))
      );
    }
    const url = `${this.apiBaseUrl}${
      this.apiBasePath
    }/api/v1/blocktime/strikes/page?page=${pageNo}&filter=${encodeURI(
      JSON.stringify(filter)
    )}`;

    return this.httpClient.get<PaginationResponse<BlockTimeStrikePublic>>(url, {
      headers: { 'Content-Type': 'application/json' },
    });
  }

  $guessableStrikesWithFilter(
    pageNo: number,
    filter: any | {},
    spanSize?: number
  ): Observable<PaginationResponse<BlockTimeStrikePublic>> {
    return this.$strikesWithFilter({ ...filter, class: 'guessable' }, pageNo, spanSize);
  }

  $outcomeKnownStrikesWithFilter(
    pageNo: number,
    filter: any | {},
    spanSize?: number
  ): Observable<PaginationResponse<BlockTimeStrikePublic>> {
    return this.$strikesWithFilter(filter, pageNo, spanSize);
  }

  $strikesGuessesWithFilter(
    filter: any | {},
    pageNo = 0
  ): Observable<PaginationResponse<BlockTimeStrikeGuessPublic>> {
    const url = `${this.apiBaseUrl}${
      this.apiBasePath
    }/api/v1/blocktime/strikes/guesses/page?page=${pageNo}&filter=${encodeURIComponent(
      JSON.stringify(filter)
    )}`;

    return this.httpClient.get<PaginationResponse<BlockTimeStrikeGuessPublic>>(
      url,
      {
        headers: { 'Content-Type': 'application/json' },
      }
    );
  }

  $strikeGuessesWithFilter(
    blockHeight: number,
    strikeMediantime: number,
    pageNo: number,
    filter: any | {}
  ): Observable<PaginationResponse<BlockTimeStrikeGuessPublic>> {
    const url = `${this.apiBaseUrl}${
      this.apiBasePath
    }/api/v1/blocktime/strike/guesses/page/${blockHeight}/${strikeMediantime}?page=${pageNo}&filter=${encodeURI(
      JSON.stringify(filter)
    )}`;

    return this.httpClient.get<PaginationResponse<BlockTimeStrikeGuessPublic>>(
      url,
      {
        headers: { 'Content-Type': 'application/json' },
      }
    );
  }

  $strike(
    blockHeight: number,
    strikeMediantime: number,
    spanSize?: number
  ): Observable<BlockTimeStrikePublic> {
    if (environment.useV2StrikesApi) {
      const resolvedSpanSize = this.resolveSpanSize(spanSize);
      const url = `${this.apiBaseUrl}${this.apiBasePath}/api/v2/strikes/blockrate/strike/${blockHeight}/${strikeMediantime}?spanSize=${resolvedSpanSize}`;
      return this.httpClient.get<BlockSpanTimeStrike>(url, {
        headers: { 'Content-Type': 'application/json' },
      }).pipe(
        map(strike => ({
          guessesCount: strike.guessesCount,
          strike: {
            block: strike.block,
            strikeMediantime: strike.mediantime,
            creationTime: strike.creationTime,
            observedResult: strike.observedResult,
            observedBlockMediantime: strike.observedBlockMediantime,
            observedBlockHash: strike.observedBlockHash,
            observedBlockHeight: strike.observedBlockHeight,
          }
        }))
      );
    }
    const url = `${this.apiBaseUrl}${this.apiBasePath}/api/v1/blocktime/strike/${blockHeight}/${strikeMediantime}`;

    return this.httpClient.get<BlockTimeStrikePublic>(url, {
      headers: { 'Content-Type': 'application/json' },
    });
  }

  $strikeGuess(
    blockHeight: number,
    strikeMediantime: number,
    guess: 'slow' | 'fast',
    spanSize?: number
  ): Observable<BlockTimeStrikeGuessPublic> {
    if (environment.useV2StrikesApi) {
      const resolvedSpanSize = this.resolveSpanSize(spanSize);
      const url = `${this.apiBaseUrl}${this.apiBasePath}/api/v2/strikes/blockrate/strike/${blockHeight}/${strikeMediantime}/guess/${guess}?spanSize=${resolvedSpanSize}`;
      return this.httpClient.post<BlockTimeStrikeGuessPublic>(url, null, {
        headers: { 'Content-Type': 'application/json' },
      });
    }
    const url = `${this.apiBaseUrl}${this.apiBasePath}/api/v1/blocktime/strike/guess/${blockHeight}/${strikeMediantime}/${guess}`;

    return this.httpClient.post<BlockTimeStrikeGuessPublic>(url, null, {
      headers: { 'Content-Type': 'application/json' },
    });
  }

  $getStrikeGuessesSummary(
    blockHeight: number,
    strikeMediantime: number,
    spanSize?: number
  ): Observable<BlockSpanTimeStrikeGuessesSummary> {
    const resolvedSpanSize = this.resolveSpanSize(spanSize);
    const url = `${this.apiBaseUrl}${this.apiBasePath}/api/v2/strikes/blockrate/strike/${blockHeight}/${strikeMediantime}/guesses/summary?spanSize=${resolvedSpanSize}`;
    return this.httpClient.get<BlockSpanTimeStrikeGuessesSummary>(url, {
      headers: { 'Content-Type': 'application/json' },
    });
  }

  $strikeGuessPerson(
    blockHeight: number,
    strikeMediantime: number,
    spanSize?: number
  ): Observable<BlockTimeStrikeGuessPublic> {
    if (environment.useV2StrikesApi) {
      const resolvedSpanSize = this.resolveSpanSize(spanSize);
      const url = `${this.apiBaseUrl}${this.apiBasePath}/api/v2/strikes/blockrate/strike/${blockHeight}/${strikeMediantime}/guess?spanSize=${resolvedSpanSize}`;
      return this.httpClient.get<BlockTimeStrikeGuessPublic>(url, {
        headers: { 'Content-Type': 'application/json' },
      });
    }
    const url = `${this.apiBaseUrl}${this.apiBasePath}/api/v1/blocktime/strike/guess/${blockHeight}/${strikeMediantime}`;

    return this.httpClient.get<BlockTimeStrikeGuessPublic>(url, {
      headers: { 'Content-Type': 'application/json' },
    });
  }
}
