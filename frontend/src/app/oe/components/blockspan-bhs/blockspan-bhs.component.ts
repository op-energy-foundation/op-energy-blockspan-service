import { Component, OnInit } from '@angular/core';
import { Block } from '../../interfaces/oe-energy.interface';
import { BlockTypes, Logos } from '../../types/constant';
import { ActivatedRoute, ParamMap } from '@angular/router';
import { OeEnergyApiService } from '../../services/oe-energy.service';
import { OeStateService } from '../../services/state.service';
import { ToastrService } from 'ngx-toastr';
import {
  catchError,
  combineLatest,
  of,
  Subscription,
  switchMap,
  take,
} from 'rxjs';
import {
  getEmptyBlockHeader,
  getHexValue,
  toScientificNotation,
} from '../../utils/helper';

@Component({
  selector: 'app-blockspan-bhs',
  templateUrl: './blockspan-bhs.component.html',
  styleUrls: ['./blockspan-bhs.component.scss'],
})
export class BlockspanBHSComponent implements OnInit {
  logos = Logos;
  isLoadingBlock = true;
  subscription: Subscription;
  fromBlock: Block;
  toBlock: Block;
  latestBlock: Block;

  constructor(
    private route: ActivatedRoute,
    private oeEnergyApiService: OeEnergyApiService,
    private stateService: OeStateService,
    private toastr: ToastrService
  ) {}

  ngOnInit() {
    (this.subscription = this.route.queryParamMap
      .pipe(
        switchMap((params: ParamMap) =>
          this.stateService.latestReceivedBlock$
            .pipe(take(1)) // don't follow any future update of this object
            .pipe(
              switchMap((block: Block) => {
                this.latestBlock = block;
                return of(params);
              })
            )
        )
      )
      .pipe(
        switchMap((params: ParamMap) => {
          const startBlock = params.get('startblock');
          const endBlock = params.get('endblock');

          const fromBlockHeight: number =
            endBlock && startBlock
              ? parseInt(startBlock, 10)
              : endBlock
              ? parseInt(endBlock, 10) - 14
              : parseInt(startBlock, 10) || this.latestBlock.height;

          const toBlockHeight: number =
            parseInt(endBlock, 10) || (startBlock ? +startBlock + 13 : 1200000);

          this.fromBlock = undefined;
          this.toBlock = undefined;

          this.isLoadingBlock = true;

          return combineLatest([
            this.oeEnergyApiService
              .$getBlockByHeight(fromBlockHeight)
              .pipe(catchError(() => of(getEmptyBlockHeader(fromBlockHeight)))),
            this.oeEnergyApiService
              .$getBlockByHeight(toBlockHeight)
              .pipe(catchError(() => of(getEmptyBlockHeader(toBlockHeight)))),
          ]);
        })
      )
      .subscribe(([fromBlock, toBlock]: [Block, Block]) => {
        this.fromBlock = fromBlock;
        this.toBlock = toBlock;
        this.isLoadingBlock = false;
      })),
      (error) => {
        this.toastr.error(`Strikes Failed To Fetch: ${error.error}`, 'Failed!');
        this.isLoadingBlock = false;
      };
  }

  convertToUTC(unixTimestamp: number): string {
    const date = new Date(unixTimestamp * 1000); // Convert seconds to milliseconds

    const year = date.getUTCFullYear();
    const month = String(date.getUTCMonth() + 1).padStart(2, '0'); // Months are 0-based, so add 1
    const day = String(date.getUTCDate()).padStart(2, '0');
    const hours = String(date.getUTCHours()).padStart(2, '0');
    const minutes = String(date.getUTCMinutes()).padStart(2, '0');
    const seconds = String(date.getUTCSeconds()).padStart(2, '0');

    return `${year}-${month}-${day} ${hours}:${minutes}:${seconds}`;
  }

  // Method to calculate the time difference in HH:MM format
  calculateTimeDifference(fromTimestamp: number, toTimestamp: number): string {
    const differenceInSeconds = toTimestamp - fromTimestamp;

    const hours = Math.floor(differenceInSeconds / 3600); // 3600 seconds in an hour
    const minutes = Math.floor((differenceInSeconds % 3600) / 60); // Get remaining minutes

    // Return formatted time difference in HH:MM
    return `${String(hours).padStart(2, '0')}:${String(minutes).padStart(
      2,
      '0'
    )}`;
  }

  getSpan(type: string): string {
    if (!this.fromBlock || !this.toBlock) return '?';

    if (type === 'blockspan') {
      return (this.toBlock.height - this.fromBlock.height).toString();
    }

    if (type === 'time') {
      return (this.toBlock.mediantime - this.fromBlock.mediantime).toString();
    }

    if (type === 'hashes') {
      return toScientificNotation(
        getHexValue(this.toBlock.chainwork) -
          getHexValue(this.fromBlock.chainwork)
      );
    }

    if (type === 'satoshis') {
      return '?';
    }

    return '?';
  }

  getBlockRate(): string {
    // Ensure fromBlock and toBlock are valid
    if (!this.fromBlock || !this.toBlock) {
      return '?';
    }

    // Retrieve values from getSpan for 'blockspan' and 'time'
    const blockspan = +this.getSpan('blockspan');
    const time = +this.getSpan('time');

    // Check if the values are valid numbers and time is not zero to avoid NaN or Infinity
    if (isNaN(blockspan) || isNaN(time) || time === 0) {
      return '?'; // Return '?' if the calculation cannot be performed
    }

    // Perform the calculation and ensure it's valid
    return ((600 * 100 * blockspan) / time).toFixed(2);
  }

  getHashRate(): string {
    // Ensure fromBlock and toBlock are valid
    if (!this.fromBlock || !this.toBlock) {
      return '?';
    }

    // Retrieve values from getSpan for 'hashes' and 'time'
    const hashes = +this.getSpan('hashes');
    const time = +this.getSpan('time');

    // Check if the values are valid numbers and time is not zero to avoid NaN or Infinity
    if (isNaN(hashes) || isNaN(time) || time === 0) {
      return '?'; // Return '?' if the calculation cannot be performed
    }

    // Perform the calculation and ensure it's valid
    return toScientificNotation(hashes / time);
  }

  getSathash(): string {
    // Ensure fromBlock and toBlock are valid
    if (!this.fromBlock || !this.toBlock) {
      return '?';
    }

    // Retrieve values from getSpan for 'hashes' and 'satoshis'
    const hashes = +this.getSpan('hashes');
    const satoshis = +this.getSpan('satoshis');

    // Check if the values are valid numbers and satoshis is not zero to avoid NaN or Infinity
    if (isNaN(hashes) || isNaN(satoshis) || satoshis === 0) {
      return '?'; // Return '?' if the calculation cannot be performed
    }

    // Perform the calculation and ensure it's valid
    return (hashes / satoshis).toFixed(2);
  }

  getChainWork(hexValue: string): string {
    // Ensure hexValue are valid
    if (!hexValue) {
      return '?';
    }

    return toScientificNotation(getHexValue(hexValue));
  }
}
