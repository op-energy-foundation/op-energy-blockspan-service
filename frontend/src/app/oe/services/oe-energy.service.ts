import { Injectable } from '@angular/core';
import { BehaviorSubject, Observable, throwError } from 'rxjs';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import {
  TimeStrike,
  TimeStrikesHistory,
  EnergyNbdrStatistics,
  BlockSpan,
  BlockHeader,
  Block,
  SwaggerJson,
  BackendGitHash,
  BlockSpanHeadersNbdr,
  BlockSpanHeaders,
  RegisterResult,
  BlockTimeStrikePublic,
  BlockTimeStrikeGuessPublic,
  PaginationResponse,
} from '../interfaces/oe-energy.interface';
import { take, switchMap, tap, shareReplay, catchError } from 'rxjs/operators';
import { OeStateService } from './state.service';
import { CookieService } from 'ngx-cookie-service';

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
  ): Observable<BlockSpanHeaders[]> {
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
  $login(secret: string): Observable<string> {
    let headers = new HttpHeaders({
      'Content-Type': 'application/json',
    });

    return this.httpClient.post<string>(
      this.apiBaseUrl + this.apiBasePath + '/api/v1/account/login',
      JSON.stringify(secret),
      {
        observe: 'body',
        responseType: 'json',
        headers,
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
  $cleanToken(): void {
    this.cookieService.delete('accountToken');
  }

  $saveToken(token: string): void {
    this.cookieService.set('accountToken', token);
  }

  $tokenExists(): boolean {
    return this.cookieService.check('accountToken');
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
    guess: 'slow' | 'fast'
  ): Observable<BlockTimeStrikeGuessPublic> {
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
  ): Observable<PaginationResponse<BlockTimeStrikePublic>> {
    const url = `${this.apiBaseUrl}${this.apiBasePath}/api/v1/blocktime/strikes/page?page=${pageNo}&filter=${encodeURI(JSON.stringify(filter))}`;

    return this.httpClient.get<
      PaginationResponse<BlockTimeStrikePublic>
    >(url, {
      headers: { 'Content-Type': 'application/json' },
    });
  }

  $guessableStrikesWithFilter(
    pageNo: number,
    filter: any | {}
  ): Observable<PaginationResponse<BlockTimeStrikePublic>> {
    return this.$strikesWithFilter( { ... filter, class: 'guessable'}, pageNo );
  }

  $outcomeKnownStrikesWithFilter(
    pageNo: number,
    filter: any | {}
  ): Observable<PaginationResponse<BlockTimeStrikePublic>> {
    return this.$strikesWithFilter( { ... filter, class: 'outcomeKnown'}, pageNo );
  }

  $strikesGuessesWithFilter(
    filter: any | {},
    pageNo = 0,
  ): Observable<PaginationResponse<BlockTimeStrikeGuessPublic>> {
    const url = `${this.apiBaseUrl}${this.apiBasePath}/api/v1/blocktime/strikes/guesses/page?page=${pageNo}&filter=${encodeURI(JSON.stringify(filter))}`;

    return this.httpClient.get<
      PaginationResponse<BlockTimeStrikeGuessPublic>
    >(url, {
      headers: { 'Content-Type': 'application/json' },
    });
  }

  $strikeGuessesWithFilter(
    blockHeight: number,
    strikeMediantime: number,
    pageNo: number,
    filter: any | {}
  ): Observable<PaginationResponse<BlockTimeStrikeGuessPublic>> {
    const url = `${this.apiBaseUrl}${this.apiBasePath}/api/v1/blocktime/strike/guesses/page/${blockHeight}/${strikeMediantime}?page=${pageNo}&filter=${encodeURI(JSON.stringify(filter))}`;

    return this.httpClient.get<
      PaginationResponse<BlockTimeStrikeGuessPublic>
    >(url, {
      headers: { 'Content-Type': 'application/json' },
    });
  }

  $strike(
    blockHeight: number,
    strikeMediantime: number,
  ): Observable<BlockTimeStrikePublic> {
    const url = `${this.apiBaseUrl}${this.apiBasePath}/api/v1/blocktime/strike/${blockHeight}/${strikeMediantime}`;

    return this.httpClient.get<
      BlockTimeStrikePublic
    >(url, {
      headers: { 'Content-Type': 'application/json' },
    });
  }

  $strikeGuess(
    accountToken: string,
    blockHeight: number,
    strikeMediantime: number,
  ): Observable<BlockTimeStrikeGuessPublic> {
    const url = `${this.apiBaseUrl}${this.apiBasePath}/api/v1/blocktime/strike/guess/${blockHeight}/${strikeMediantime}`;

    return this.httpClient.get<
      BlockTimeStrikeGuessPublic
    >(url, {
        headers: { 'Content-Type': 'application/json',
                   'AccountToken': accountToken,
                 }
    });
  }

  $strikeGuessPerson(
    uuid: string,
    blockHeight: number,
    strikeMediantime: number,
  ): Observable<BlockTimeStrikeGuessPublic> {
    const url = `${this.apiBaseUrl}${this.apiBasePath}/api/v1/blocktime/strike/guess/person/${uuid}/${blockHeight}/${strikeMediantime}`;

    return this.httpClient.get<
      BlockTimeStrikeGuessPublic
    >(url, {
        headers: { 'Content-Type': 'application/json',
                 }
    });
  }
}
