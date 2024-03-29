import { Injectable } from '@angular/core';
import { Observable, ReplaySubject, map } from 'rxjs';
import {
  TimeStrike,
  SlowFastGuess,
  SlowFastGuessOutcome,
  OeEnergyWebsocketResponse,
  Block,
} from '../interfaces/oe-energy.interface';
import { WebsocketService } from './websocket.service';

@Injectable({
  providedIn: 'root',
})
export class OeStateService {
  accountTokenState: 'init' | 'checked' | 'generated' = 'init'; // this flag is being used to check if frontend should ask to generate account id hash
  $showAccountURLWarning: ReplaySubject<boolean> = new ReplaySubject<boolean>(
    1
  ); // this flag is being used to check if frontend should display WARNING message
  $accountSecret: ReplaySubject<string> = new ReplaySubject(1); // this value will only be used if user haven't specified it
  $accountToken: ReplaySubject<string> = new ReplaySubject(1); // this value is an API token
  timeStrikes$ = new ReplaySubject<TimeStrike>(1);
  timeSlowFastGuesses$ = new ReplaySubject<SlowFastGuess>(1);
  timeSlowFastGuessesOutcome$ = new ReplaySubject<SlowFastGuessOutcome>(1);
  latestReceivedBlock$ = new ReplaySubject<Block>(1); // this object will only contain the last block received from the backend. This block can be considered as the current tip
  latestReceivedBlockHeight = -1; // plain  block height of the latest recevied block. Need this value to handle the case when backend sends newly found block
  latestBlockHeight = -1;
  private apiBaseUrl: string; // base URL is protocol, hostname, and port
  constructor() {
    this.apiBaseUrl = ''; // use relative URL by default
  }

  /* 
  callback which will be called by websocket service
  */
  handleWebsocketResponse(
    websocketService: WebsocketService,
    response: OeEnergyWebsocketResponse
  ): void {
    if (
      response['oe-newest-confirmed-block'] &&
      response['oe-newest-confirmed-block'].height !== undefined
    ) {
      this.latestReceivedBlockHeight =
        response['oe-newest-confirmed-block'].height;
      const block = response['oe-newest-confirmed-block'];
      this.latestReceivedBlock$.next(block);
    }

    if (response.declinedAccountSecret) {
      websocketService.want(['generatedaccounttoken']);
    }
    if (response.checkedAccountToken) {
      this.accountTokenState = 'checked';
      this.$accountToken.next(response.checkedAccountToken);
      this.$showAccountURLWarning.next(false);
    }
    if (response.generatedAccountSecret && response.generatedAccountToken) {
      this.accountTokenState = 'generated';
      this.$accountSecret.next(response.generatedAccountSecret);
      this.$accountToken.next(response.generatedAccountToken);
      this.$showAccountURLWarning.next(true);
    }
    if (response.timeStrike) {
      const ts = response.timeStrike;
      this.timeStrikes$.next(ts);
    }
    if (response.timeSlowFastGuess) {
      const slowFastGuess = response.timeSlowFastGuess;
      this.timeSlowFastGuesses$.next(slowFastGuess);
    }
    if (response.timeSlowFastGuessOutcome) {
      const slowFastGuessOutcome = response.timeSlowFastGuessOutcome;
      this.timeSlowFastGuessesOutcome$.next(slowFastGuessOutcome);
    }
  }

  generateAccountAccessURL(): Observable<string> {
    return this.$accountSecret.pipe(
      map(
        (secret) =>
          `${document.location.protocol}${document.location.host}/login/${secret}`
      )
    );
  }

  // Method to update the account secret
  updateAccountSecret(secret: string): void {
    this.$accountSecret.next(secret);
  }

  // Method to update the account token
  updateAccountToken(token: string): void {
    this.$accountToken.next(token);
  }

  // Optionally, if you want to handle accountTokenState updates
  updateAccountTokenState(state: 'init' | 'checked' | 'generated'): void {
    this.accountTokenState = state;
  }

  // Method to trigger the warning message
  showAccountURLWarning(show: boolean): void {
    this.$showAccountURLWarning.next(show);
  }
}
