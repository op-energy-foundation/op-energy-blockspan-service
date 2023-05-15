import { Injectable } from '@angular/core';
import { webSocket, WebSocketSubject } from 'rxjs/webSocket';
import { Subscription } from 'rxjs';
import { take } from 'rxjs/operators';
import { TransferState, makeStateKey } from '@angular/platform-browser';
import {
  Block,
  OpEnergyWebsocketResponse,
  WebsocketResponse,
} from '../interfaces/op-energy.interface';
import { OeStateService } from './state.service';
import { environment } from '../../../environments/environment';

const OFFLINE_RETRY_AFTER_MS = 1000;
const OFFLINE_PING_CHECK_AFTER_MS = 10000;
const EXPECT_PING_RESPONSE_AFTER_MS = 5000;

@Injectable({
  providedIn: 'root',
})
export class WebsocketService {
  private webSocketProtocol =
    document.location.protocol === 'https:' ? 'wss:' : 'ws:';
  private webSocketUrl =
    this.webSocketProtocol +
    '//' +
    environment.baseUrl.replace(/^https?:\/\//i, '') +
    '/api/v1/ws';
  private websocketSubject: WebSocketSubject<WebsocketResponse>;
  private goneOffline = false;
  private lastWant: string | null = null;
  private onlineCheckTimeout: number;
  private onlineCheckTimeoutTwo: number;
  private subscription: Subscription;

  constructor(private opEnergyStateService: OeStateService) {
    this.websocketSubject = webSocket<WebsocketResponse>(this.webSocketUrl);
    this.startSubscription();
  }

  startSubscription(retrying = false, hasInitData = false) {
    if (!hasInitData) {
      this.websocketSubject.next({ action: 'init' });
    }
    if (retrying) {
      // this.stateService.connectionState$.next(1);
    }
    this.subscription = this.websocketSubject.subscribe(
      (response: OpEnergyWebsocketResponse) => {
        this.handleResponse(response);

        if (this.goneOffline === true) {
          this.goneOffline = false;
          if (this.lastWant) {
            this.want(JSON.parse(this.lastWant), true);
          }
          // this.stateService.connectionState$.next(2);
        }

        // if (this.stateService.connectionState$.value === 1) {
        //   this.stateService.connectionState$.next(2);
        // }

        this.startOnlineCheck();
      },
      (err: Error) => {
        console.log(err);
        console.log(
          `WebSocket error, trying to reconnect in ${OFFLINE_RETRY_AFTER_MS} seconds`
        );
        this.goOffline();
      }
    );
  }

  want(data: string[], force = false) {
    if (JSON.stringify(data) === this.lastWant && !force) {
      return;
    }
    this.websocketSubject.next({ action: 'want', data: data });
    this.lastWant = JSON.stringify(data);
  }

  goOffline() {
    this.goneOffline = true;
    // this.stateService.connectionState$.next(0);
    window.setTimeout(() => {
      this.startSubscription(true);
    }, OFFLINE_RETRY_AFTER_MS);
  }

  startOnlineCheck() {
    clearTimeout(this.onlineCheckTimeout);
    clearTimeout(this.onlineCheckTimeoutTwo);

    this.onlineCheckTimeout = window.setTimeout(() => {
      this.websocketSubject.next({ action: 'ping' });
      this.onlineCheckTimeoutTwo = window.setTimeout(() => {
        if (!this.goneOffline) {
          console.log(
            'WebSocket response timeout, force closing, trying to reconnect in 10 seconds'
          );
          this.websocketSubject.complete();
          this.subscription.unsubscribe();
          this.goOffline();
        }
      }, EXPECT_PING_RESPONSE_AFTER_MS);
    }, OFFLINE_PING_CHECK_AFTER_MS);
  }

  handleResponse(response: OpEnergyWebsocketResponse) {
    // op-energy hook
    this.opEnergyStateService.handleWebsocketResponse(this, response);

    if (response.blocks && response.blocks.length) {
      const blocks = response.blocks;
      blocks.forEach((block: Block) => {
        if (block.height > this.opEnergyStateService.latestBlockHeight) {
          this.opEnergyStateService.latestBlockHeight = block.height;
        }
      });
    }

    if (response.block) {
      if (response.block.height > this.opEnergyStateService.latestBlockHeight) {
        this.opEnergyStateService.latestBlockHeight = response.block.height;
      }
    }
  }
}
