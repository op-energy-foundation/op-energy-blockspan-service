import { Component, OnInit } from '@angular/core';
import {
  Block,
  BlockTimeStrike,
  BlockTimeStrikePublic,
  PaginationResponse,
} from '../../interfaces/oe-energy.interface';
import { BlockTypes, Logos } from '../../types/constant';
import { ActivatedRoute, ParamMap } from '@angular/router';
import {
  OeBlocktimeApiService,
  OeEnergyApiService,
} from '../../services/oe-energy.service';
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
  selector: 'app-block-rate-strike-details-v2',
  templateUrl: './block-rate-strike-details-v2.component.html',
  styleUrls: ['./block-rate-strike-details-v2.component.scss'],
})
export class BlockRateStrikeDetailsV2Component implements OnInit {
  logos = Logos;
  isLoadingBlock = true;
  subscription: Subscription;
  strike: BlockTimeStrike = {} as BlockTimeStrike;
  fromBlock: Block;
  toBlock: Block;
  latestBlock: Block;
  disabled: boolean = false;
  isSelected: boolean = false;
  selectedGuess: string;
  strikeKnown = false;
  color = 'red';

  constructor(
    private route: ActivatedRoute,
    private oeEnergyApiService: OeEnergyApiService,
    private stateService: OeStateService,
    private oeBlocktimeApiService: OeBlocktimeApiService,
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
          let fromBlockHeight = +params.get('blockspanStart');
          const strikeHeight = +params.get('strikeHeight') || 1200000;
          let strikeTime = +params.get('strikeTime');
          if (!strikeTime) {
            strikeTime =
              this.latestBlock.mediantime +
              (strikeHeight - this.latestBlock.height) * 600;
          }

          if (!fromBlockHeight) {
            fromBlockHeight = Math.max(0, strikeHeight - 14);
          }
          // Creating temporary strike
          this.strike = {
            block: strikeHeight,
            strikeMediantime: strikeTime,
            creationTime: undefined,
          };

          this.isLoadingBlock = true;

          return combineLatest([
            this.oeEnergyApiService
              .$getBlockByHeight(fromBlockHeight)
              .pipe(catchError(() => of(getEmptyBlockHeader(fromBlockHeight)))),
            this.oeEnergyApiService
              .$getBlockByHeight(strikeHeight)
              .pipe(catchError(() => of(getEmptyBlockHeader(strikeHeight)))),
            this.oeBlocktimeApiService
              .$strikesWithFilter({
                strikeMediantimeEQ: strikeTime,
                blockHeightEQ: strikeHeight,
              })
              .pipe(catchError(() => of(strikeHeight))),
          ]);
        })
      )
      .subscribe(
        ([fromBlock, toBlock, strikesDetails]: [
          Block,
          Block,
          PaginationResponse<BlockTimeStrikePublic>
        ]) => {
          this.fromBlock = fromBlock;
          if (typeof toBlock === BlockTypes.NUMBER) {
            this.toBlock = {
              ...this.fromBlock,
              height: +toBlock,
            };
          } else {
            this.toBlock = toBlock;
          }
          const strikesResult = strikesDetails.results;
          if (!strikesResult.length) {
            this.toastr.error('Strikes Not Found!', 'Failed!');
            return;
          }

          this.strike = {
            ...strikesResult[0].strike,
            block: strikesResult[0].strike.block,
            creationTime: strikesResult[0].strike.creationTime,
            strikeMediantime: strikesResult[0].strike.strikeMediantime,
            observedResult: strikesResult[0].strike.observedResult,
          };

          this.isLoadingBlock = false;
          this.checkExistingGuess();
        }
      )),
      (error) => {
        this.toastr.error(`Strikes Failed To Fetch: ${error.error}`, 'Failed!');
        this.isLoadingBlock = false;
      };
  }

  convertToUTC(unixTimestamp: number): string {
    if (unixTimestamp === 0) {
      return '?';
    }

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
    if (fromTimestamp === 0 || toTimestamp === 0) return '?';

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
      return !this.fromBlock.mediantime || !this.toBlock.mediantime
        ? '?'
        : (this.toBlock.mediantime - this.fromBlock.mediantime).toString();
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

  selectGuessingBox(): void {}

  getResult(): string {
    if (!this.strike.observedBlockHeight) return;

    const heightOverStrikeHeight = this.getJudgementHeight();
    const timeOverStrikeTime = this.getJudgementTime();

    if (heightOverStrikeHeight && !timeOverStrikeTime) return 'fast';

    if (!heightOverStrikeHeight && timeOverStrikeTime) return 'slow';

    return 'slow';
  }

  getJudgementResult(): string {
    const result = this.getResult();

    if (result === 'fast') {
      return 'faster';
    }

    if (result === 'slow') {
      return 'slower';
    }

    return '?';
  }

  handleSelectedGuess(selected: 'slow' | 'fast'): void {
    // this.isSelected = true;
    this.selectedGuess = selected;
    this.oeBlocktimeApiService
      .$strikeGuess(this.strike.block, this.strike.strikeMediantime, selected)
      .subscribe(
        (response) => {
          this.disabled = true;
          this.toastr.success('Successfully added guess', 'Success');
        },
        (error) => {
          this.toastr.error(
            'Failed to add guess. Error: ' + error.error,
            'Failed!'
          );
        }
      );
  }

  checkExistingGuess(): void {
    this.isLoadingBlock = true;
    this.oeBlocktimeApiService
      .$strikeGuessPerson(this.strike.block, this.strike.strikeMediantime)
      .subscribe(
        (response) => {
          this.isLoadingBlock = false;
          this.disabled = true;
          this.toastr.warning(
            'You already have a guess: ' + response.guess,
            'Warning'
          );
          this.selectedGuess = response.guess;
        },
        (error) => {
          // DOING NOTHING
          if (this.strike.observedResult) {
            //disabling strike as strike outcome is known
            this.disabled = true;
            this.strikeKnown = true;
            this.toastr.warning(
              "Can't add guess as stike outcome is known.",
              'Warning'
            );
          }
          this.isLoadingBlock = false;
        }
      );
  }

  getJudgementHeight(): boolean {
    if (!this.strike.observedBlockHeight) {
      return;
    }

    return this.strike.observedBlockHeight > this.strike.block - 1;
  }

  getJudgementTime(): boolean {
    if (!this.strike.observedBlockMediantime) {
      return;
    }

    return this.strike.observedBlockMediantime > this.strike.strikeMediantime;
  }
}
