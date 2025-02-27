// Define a base class for shared logic across components
import { ActivatedRoute, ParamMap, Router } from '@angular/router';
import {
  catchError,
  combineLatest,
  Observable,
  of,
  Subscription,
  take,
} from 'rxjs';
import { ToastrService } from 'ngx-toastr';
import {
  OeBlocktimeApiService,
  OeEnergyApiService,
} from '../../../services/oe-energy.service';
import { OeStateService } from '../../../services/state.service';
import {
  Block,
  BlockTimeStrike,
  BlockTimeStrikePublic,
  PaginationResponse,
} from '../../../interfaces/oe-energy.interface';
import {
  getEmptyBlockHeader,
  getHexValue,
  toScientificNotation,
  calculateTimeDifference,
  convertToUTC
} from '../../../utils/helper';

export abstract class BaseBlockComponent {
  fromBlock: Block;
  toBlock: Block;
  latestBlock: Block;
  isLoadingBlock = true;
  subscription: Subscription;
  calculateTimeDifference = calculateTimeDifference;
  convertToUTC = convertToUTC;
  strike: BlockTimeStrike = {} as BlockTimeStrike;

  constructor(
    protected router: Router,
    protected oeEnergyApiService: OeEnergyApiService,
    protected oeBlocktimeApiService: OeBlocktimeApiService,
    protected stateService: OeStateService,
    protected toastr: ToastrService
  ) {}

  // Shared method to initialize block data
  initializeBlocks(
    fromBlockHeight: number,
    strikeHeight?: number,
    strikeTime?: number
  ): Observable<(Block | PaginationResponse<BlockTimeStrikePublic> | null)[]> {
    this.isLoadingBlock = true;

    const observables: Observable<
      Block | PaginationResponse<BlockTimeStrikePublic> | null
    >[] = [
      this.oeEnergyApiService
        .$getBlockByHeight(fromBlockHeight)
        .pipe(catchError(() => of(getEmptyBlockHeader(fromBlockHeight)))),
    ];

    if (strikeHeight !== undefined) {
      observables.push(
        this.oeEnergyApiService
          .$getBlockByHeight(strikeHeight)
          .pipe(catchError(() => of(getEmptyBlockHeader(strikeHeight))))
      );
    }

    if (strikeTime !== undefined && strikeHeight !== undefined) {
      observables.push(
        this.oeBlocktimeApiService
          .$strikesWithFilter({
            strikeMediantimeEQ: strikeTime,
            blockHeightEQ: strikeHeight,
          })
          .pipe(catchError(() => of(null)))
      );
    }

    return combineLatest(observables);
  }

  // Shared method for processing query parameters with defaults
  processQueryParams(route: ActivatedRoute): { [key: string]: any } {
    const defaultParams = {
      blockspanStart: undefined,
      startblock: undefined,
      endblock: this.latestBlock?.height || undefined,
      strikeHeight: 1200000,
      strikeTime: undefined,
    };
    const params: { [key: string]: any } = { ...defaultParams };
    route.queryParamMap.pipe(take(1)).subscribe((paramMap: ParamMap) => {
      Object.keys(defaultParams).forEach((key) => {
        params[key] = paramMap.get(key) || defaultParams[key];
      });
    });
    return params;
  }

  // Shared method to get the latest block from state
  async initializeLatestBlock(): Promise<void> {
    this.latestBlock = (await this.stateService.latestReceivedBlock$
      .pipe(take(1))
      .toPromise()) as Block;
  }

  // Shared method to calculate fromBlockHeight and toBlockHeight
  calculateBlockRange(params: {
    startBlock: number;
    endBlock: number;
    strikeTime?: number;
  }): {
    fromBlockHeight: number;
    toBlockHeight: number;
    strikeTime: number;
  } {
    if (!this.latestBlock) {
      throw new Error('Latest block is not initialized');
    }

    const { startBlock, endBlock } = params;

    if (startBlock > this.latestBlock.height) {
      this.toastr.error('Viewing requires known start block', 'Failed!');
      throw new Error('Invalid start block');
    }

    if (startBlock >= endBlock) {
      this.toastr.error('Start block must be less than end block', 'Failed!');
      throw new Error('Invalid block range');
    }

    const fromBlockHeight =
      endBlock && startBlock
        ? startBlock
        : endBlock
        ? endBlock - 2016
        : startBlock || this.latestBlock.height;

    const toBlockHeight =
      endBlock || (startBlock ? startBlock + 2016 : 1200000);

    let strikeTime = params.strikeTime;

    if (!strikeTime) {
      strikeTime =
        this.latestBlock.mediantime +
        (endBlock - this.latestBlock.height) * 600;
    }

    return { fromBlockHeight, toBlockHeight, strikeTime };
  }

  // Shared error handler
  handleError(message: string): void {
    this.toastr.error(message, 'Failed!');
  }

  // Lifecycle management
  cleanup(): void {
    if (this.subscription) {
      this.subscription.unsubscribe();
    }
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

    if (type === 'striketime') {
      return !this.fromBlock.mediantime || !this.strike.strikeMediantime
        ? '?'
        : (this.strike.strikeMediantime - this.fromBlock.mediantime).toString();
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

  getBlockRate(type: string = 'time'): string {
    // Ensure fromBlock and toBlock are valid
    if (!this.fromBlock || !this.toBlock) {
      return '?';
    }

    // Retrieve values from getSpan for 'blockspan' and 'time'
    const blockspan = +this.getSpan('blockspan');
    const time = +this.getSpan(type);

    // Check if the values are valid numbers and time is not zero to avoid NaN or Infinity
    if (isNaN(blockspan) || isNaN(time) || time === 0) {
      return '?'; // Return '?' if the calculation cannot be performed
    }

    // Perform the calculation and ensure it's valid
    return ((600 * 100 * blockspan) / time).toFixed(2);
  }

  goToStrikeDetails(event: Event): string {
    if (window.getSelection()?.toString()) {
      // If there is selected text, prevent the click event from propagating
      event.stopPropagation();
      return;
    }

    const queryParams: any = {
      startblock: this.fromBlock.height,
      endblock: this.toBlock.height,
    };

    // Navigate to the target route with the query parameters
    this.router.navigate(['/hashstrikes/blockspan-details'], { queryParams });
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

  isBlockRunningFaster(): boolean {
    const result = this.getBlockRate();

    return result !== '?' && Number(result) > 100;
  }
}
