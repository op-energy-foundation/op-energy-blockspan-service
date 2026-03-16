import { Component, Input, OnInit } from '@angular/core';
import {
  Block,
  BlockTimeStrike,
  BlockTimeStrikePublic,
  PaginationResponse,
} from '../../interfaces/oe-energy.interface';
import { APP_CONFIGURATION, BlockTypes, Logos } from '../../types/constant';
import { ActivatedRoute, ParamMap, Router } from '@angular/router';
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
  map,
} from 'rxjs';
import {
  getEmptyBlockHeader,
  getHexValue,
  toScientificNotation,
} from '../../utils/helper';

@Component({
  selector: 'app-blockrate-strike-summary-v2',
  templateUrl: './blockrate-strike-summary-v2.component.html',
  styleUrls: ['./blockrate-strike-summary-v2.component.scss'],
})
export class BlockrateStrikeSummaryV2Component implements OnInit {
  logos = Logos;
  isLoadingBlock = true;
  subscription: Subscription;
  @Input() strike: BlockTimeStrike = {} as BlockTimeStrike;
  @Input() fromBlock: Block;
  @Input() fromBlockHeight: number;
  @Input() toBlock: Block;
  @Input() toBlockHeight: number;
  @Input() strikeTime: number;
  @Input() existingGuess: 'slow' | 'fast';
  @Input() preloaded = false;
  latestBlock: Block;
  disabled: boolean = false;
  isSelected: boolean = false;
  selectedGuess: string;
  strikeKnown = false;
  color = 'red';

  constructor(
    private router: Router,
    private route: ActivatedRoute,
    private oeEnergyApiService: OeEnergyApiService,
    private stateService: OeStateService,
    private oeBlocktimeApiService: OeBlocktimeApiService,
    private toastr: ToastrService
  ) {}

  ngOnInit() {
    if (this.strike?.block && this.fromBlock && this.toBlock) {
      this.isLoadingBlock = false;
      this.applyExistingGuess();
      return;
    }

    if (this.fromBlock && this.toBlock && this.strikeTime) {
      this.strike = {
        block: this.toBlock.height,
        strikeMediantime: this.strikeTime,
        creationTime: undefined,
      };
      this.fetchStrikeDetails();
      return;
    }
    
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
          let fromBlockHeight = this.fromBlockHeight || +params.get('startblock');
          const strikeHeight = this.toBlockHeight || +params.get('strikeHeight') || 1200000;
          let strikeTime = this.strikeTime || +params.get('strikeTime');

          if (fromBlockHeight >= strikeHeight) {
            this.toastr.error(
              'Start block must be less than strike height',
              'Failed!'
            );
            return of(null);
          }

          if (fromBlockHeight > this.latestBlock.height) {
            this.fromBlock = getEmptyBlockHeader(fromBlockHeight) as Block;
            this.toBlock = getEmptyBlockHeader(strikeHeight) as Block;
            this.strike = {
              block: strikeHeight,
              strikeMediantime: strikeTime,
              creationTime: undefined,
            };
            this.isLoadingBlock = false;
            this.checkExistingGuess();
            return of(null);
          }

          if (!strikeTime) {
            strikeTime =
              this.latestBlock.mediantime +
              (strikeHeight - this.latestBlock.height) * 600;
          }

          if (!fromBlockHeight) {
            fromBlockHeight = Math.max(0, strikeHeight - APP_CONFIGURATION.SPAN_SIZE);
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
              .$getBlocksByHeights([fromBlockHeight, strikeHeight]),
            this.oeBlocktimeApiService
              .$strikesWithFilter({
                strikeMediantimeEQ: strikeTime,
                blockHeightEQ: strikeHeight,
              })
              .pipe(catchError(() => of(strikeHeight))),
          ]).pipe(
            map(([blocks, strikes]) => [blocks, strikes] as [Block[], PaginationResponse<BlockTimeStrikePublic> | number])
          );
        })
      )
      .subscribe(
        (result: any) => {
          if (!result) return;
          const [blocks, strikesDetails] = result as [Block[], PaginationResponse<BlockTimeStrikePublic>];
          const [fromBlock, toBlock] = blocks;
          this.fromBlock = fromBlock;
          if (typeof toBlock === BlockTypes.NUMBER) {
            this.toBlock = {
              ...this.fromBlock,
              height: +toBlock,
            };
          } else {
            this.toBlock = toBlock;
          }
          if (strikesDetails?.results?.length) {
            const strikesResult = strikesDetails.results;
            this.strike = {
              ...strikesResult[0].strike,
              block: strikesResult[0].strike.block,
              creationTime: strikesResult[0].strike.creationTime,
              strikeMediantime: strikesResult[0].strike.strikeMediantime,
              observedResult: strikesResult[0].strike.observedResult,
            };
          }

          this.isLoadingBlock = false;
          this.checkExistingGuess();
        }
      )),
      (error) => {
        this.toastr.error(`Strikes Failed To Fetch: ${error.error}`, 'Failed!');
        this.isLoadingBlock = false;
      };
  }

  private fetchStrikeDetails(): void {
    this.isLoadingBlock = true;
    this.oeBlocktimeApiService
      .$strikesWithFilter({
        strikeMediantimeEQ: this.strikeTime,
        blockHeightEQ: this.toBlock.height,
      })
      .pipe(catchError(() => of(null)))
      .subscribe((strikesDetails: PaginationResponse<BlockTimeStrikePublic>) => {
        if (strikesDetails?.results?.length) {
          const result = strikesDetails.results[0];
          this.strike = {
            ...result.strike,
            block: result.strike.block,
            creationTime: result.strike.creationTime,
            strikeMediantime: result.strike.strikeMediantime,
            observedResult: result.strike.observedResult,
          };
        }
        this.isLoadingBlock = false;
        this.checkExistingGuess();
      });
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

  getResult(): string {
    if (!this.strike.observedBlockHeight) return;

    const heightOverStrikeHeight = this.getJudgementHeight();
    const timeOverStrikeTime = this.getJudgementTime();

    if (heightOverStrikeHeight && !timeOverStrikeTime) return 'fast';

    if (!heightOverStrikeHeight && timeOverStrikeTime) return 'slow';

    return 'slow';
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

  private applyExistingGuess(): void {
    if (this.existingGuess) {
      this.disabled = true;
      this.selectedGuess = this.existingGuess;
    }
    if (this.strike.observedResult) {
      this.disabled = true;
      this.strikeKnown = true;
    }
    if (this.preloaded) return;
    if (!this.existingGuess) {
      this.checkExistingGuess();
    }
  }

  checkExistingGuess(): void {
    this.isLoadingBlock = true;
    this.oeBlocktimeApiService
      .$strikeGuessPerson(this.strike.block, this.strike.strikeMediantime)
      .subscribe(
        (response) => {
          this.isLoadingBlock = false;
          this.disabled = true;
          if (!this.fromBlockHeight && !this.preloaded) {
            this.toastr.warning(
              'You already have a guess: ' + response.guess,
              'Warning'
            );
          }
          this.selectedGuess = response.guess;
        },
        (error) => {
          // DOING NOTHING
          if (this.strike.observedResult) {
            //disabling strike as strike outcome is known
            this.disabled = true;
            this.strikeKnown = true;
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

  goToBlockRateDetails(event: Event): string {
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

  goToStrikeDetails(event: Event): string {
    if (window.getSelection()?.toString()) {
      // If there is selected text, prevent the click event from propagating
      event.stopPropagation();
      return;
    }

    const queryParams: any = {
      strikeHeight: this.strike.block,
      strikeTime: this.strike.strikeMediantime,
      startblock: this.fromBlock.height,
    };

    // Navigate to the target route with the query parameters
    this.router.navigate(['/hashstrikes/blockrate-strike-details'], {
      queryParams,
    });
  }

  getStrikeRate(): string {
    // Ensure fromBlock and toBlock are valid
    if (!this.fromBlock || !this.strike) {
      return '?';
    }

    // Retrieve values from getSpan for 'blockspan' and 'time'
    const blockspan = +this.getSpan('blockspan');
    const time = +this.getSpan('striketime');

    // Check if the values are valid numbers and time is not zero to avoid NaN or Infinity
    if (isNaN(blockspan) || isNaN(time) || time === 0) {
      return '?'; // Return '?' if the calculation cannot be performed
    }

    // Perform the calculation and ensure it's valid
    return ((600 * 100 * blockspan) / time).toFixed(2);
  }

  getAnimal(type: string = ''): string {
    const result = this.getResult();

    if (!result && !this.selectedGuess) {
      return '?';
    }

    const value = result ? result : this.selectedGuess;
    const logo = value === 'slow' ? '🐢' : '🐰';

    if (type !== 'hashrate') {
      return logo;
    }

    return value === 'slow' ? '🐰' : '🐢';
  }
}
