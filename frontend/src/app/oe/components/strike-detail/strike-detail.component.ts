import {
  Block,
  BlockTimeStrikePublic,
  BlockTimeStrikeGuessPublic,
  PaginationResponse,
} from '../../interfaces/oe-energy.interface';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { Location } from '@angular/common';
import { ActivatedRoute, ParamMap, Router } from '@angular/router';
import { Subscription, of, combineLatest } from 'rxjs';
import { switchMap, catchError, map, take } from 'rxjs/operators';
import { NgbModal } from '@ng-bootstrap/ng-bootstrap';
import { ToastrService } from 'ngx-toastr';
import {
  TimeStrike,
} from 'src/app/oe/interfaces/oe-energy.interface';
import { BlockTypes } from '../../types/constant';
import {
  OeBlocktimeApiService,
  OeEnergyApiService,
} from '../../services/oe-energy.service';
import { OeStateService } from '../../services/state.service';

@Component({
  selector: 'app-strike-detail',
  templateUrl: './strike-detail.component.html',
  styleUrls: ['./strike-detail.component.scss'],
})
export class StrikeDetailComponent implements OnInit, OnDestroy {
  network = '';
  fromBlock: Block;
  toBlock: Block | any;
  blockHeight: number;
  nextBlockHeight: number;
  fromBlockHeight: number;
  toBlockHeight: number;
  strike: TimeStrike;
  isLoadingBlock = true;
  latestBlock: Block;
  latestBlocks: Block[] = [];
  isLoadingTransactions = true;
  error: any;
  itemsPerPage: number;
  showPreviousBlocklink = true;
  showNextBlocklink = true;

  subscription: Subscription;
  slowFastGuesses: BlockTimeStrikeGuessPublic[] = [];
  currentActiveGuess: 'slow' | 'fast' | null = null;
  guessStrike: BlockTimeStrikeGuessPublic[] = [];
  curruntPage: number = 0;

  get strikeElapsedTime(): number {
    return this.strike.strikeMediantime - this.fromBlock.mediantime;
  }

  get span(): number {
    return this.toBlock.height - this.fromBlock.height;
  }

  get timeDiff(): number {
    return this.toBlock.mediantime - this.fromBlock.mediantime;
  }

  get energyDiff(): number {
    return ((this.span * 600 - this.timeDiff) / (this.span * 600)) * 100;
  }

  get canGuess(): boolean {
    return (
      this.stateService.latestBlockHeight > 0 &&
      this.strike.blockHeight > this.stateService.latestBlockHeight
    );
  }

  get spanWithStrike(): number {
    return this.strike.blockHeight - this.fromBlock.height;
  }

  get timeDiffWithStrike(): number {
    return this.strike.strikeMediantime - this.fromBlock.mediantime;
  }

  get energyDiffWithStrike(): number {
    return (
      ((this.spanWithStrike * 600 - this.timeDiffWithStrike) /
        (this.spanWithStrike * 600)) *
      100
    );
  }

  get totalIconCountWithStrike() {
    let count = Math.round((6 + this.energyDiffWithStrike / 5) / 2);
    if (count < 0) {
      count = 0;
    }
    return count;
  }

  get strikeType(): 'Energy' | 'Strike' | 'Strike_Boiling' {
    return this.strike.strikeMediantime > this.toBlock.mediantime
      ? 'Strike_Boiling'
      : 'Strike';
  }

  constructor(
    private route: ActivatedRoute,
    private toastr: ToastrService,
    private oeEnergyApiService: OeEnergyApiService,
    private oeBlocktimeApiService: OeBlocktimeApiService,
    public stateService: OeStateService,
    private oeEnergyStateService: OeStateService
  ) {}

  ngOnInit() {
    this.subscription = this.route.queryParamMap
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
          const fromBlockHeight =
            +params.get('blockspanStart') || this.latestBlock.height;
          const strikeHeight = +params.get('strikeHeight') || 1200000;
          let strikeTime = +params.get('strikeTime');

          if (!strikeTime) {
            strikeTime =
              this.latestBlock.mediantime +
              (strikeHeight - this.latestBlock.height) * 600;
          }
          // Creating temporary strike
          this.strike = {
            blockHeight: strikeHeight,
            strikeMediantime: strikeTime,
            creationTime: undefined,
          };

          this.fromBlockHeight = fromBlockHeight;
          this.toBlockHeight = strikeHeight;
          this.blockHeight =
            history.state.data?.blockHeight ?? this.blockHeight;

          document.body.scrollTo(0, 0);

          if (history.state.data?.block) {
            this.blockHeight = history.state.data.block.height;
            return of([history.state.data.block, history.state.data.block]);
          }

          this.isLoadingBlock = true;

          const fromBlockInCache = this.latestBlocks.find(
            (block) => block.height === fromBlockHeight
          );
          const toBlockInCache = this.latestBlocks.find(
            (block) => block.height === strikeHeight
          );

          if (fromBlockInCache && toBlockInCache) {
            return of([fromBlockInCache, toBlockInCache]);
          }

          return combineLatest([
            this.oeEnergyApiService
              .$getBlockByHeight(fromBlockHeight)
              .pipe(catchError(() => of(fromBlockHeight))),
            this.oeEnergyApiService
              .$getBlockByHeight(strikeHeight)
              .pipe(catchError(() => of(strikeHeight))),
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
        async ([fromBlock, toBlock, strikesDetails]: [
          Block,
          Block,
          PaginationResponse<BlockTimeStrikePublic>
        ]) => {
          this.fromBlock = fromBlock;
          this.toBlock =
            typeof toBlock === BlockTypes.NUMBER
              ? { height: +toBlock }
              : toBlock;
          this.blockHeight = fromBlock.height;
          this.nextBlockHeight = fromBlock.height + 1;
          this.setNextAndPreviousBlockLink();

          const strikesResult = strikesDetails.results;
          if (!strikesResult.length) {
            this.toastr.error('Strikes Not Found!', 'Failed!');
            return;
          }

          this.strike = {
            blockHeight: strikesResult[0].strike.block,
            creationTime: strikesResult[0].strike.creationTime,
            strikeMediantime: strikesResult[0].strike.strikeMediantime,
          };

          // Fetching guessing data
          const strikesFilter = {
            strikeMediantimeEQ: this.strike.strikeMediantime,
            blockHeightEQ: this.strike.blockHeight,
          };
          if (this.stateService.latestReceivedBlockHeight > toBlock.height) {
            await this.fetchPastGuessData(this.curruntPage, strikesFilter);
          } else {
            await this.fetchFutureGuessData(this.curruntPage, strikesFilter);
          }
          this.isLoadingBlock = false;
        },
        (error) => {
          this.error = error;
          this.isLoadingBlock = false;
        }
      );
  }

  ngOnDestroy() {
    this.subscription.unsubscribe();
  }

  setNextAndPreviousBlockLink() {
    if (this.latestBlock && this.blockHeight) {
      if (this.blockHeight === 0) {
        this.showPreviousBlocklink = false;
      } else {
        this.showPreviousBlocklink = true;
      }
      if (
        this.latestBlock.height &&
        this.latestBlock.height === this.blockHeight
      ) {
        this.showNextBlocklink = false;
      } else {
        this.showNextBlocklink = true;
      }
    }
  }

  getHexValue(str) {
    const arr1 = str.split('');
    const idx = arr1.findIndex((a) => a !== '0');
    let hexValue = '0x';
    if (idx > -1) {
      hexValue += str.slice(idx);
    } else {
      hexValue += str;
    }
    return hexValue;
  }

  guess(guess: 'slow' | 'fast') {
    this.currentActiveGuess = guess;
    let subscription = this.oeEnergyStateService.$accountToken.subscribe(
      (accountToken) => {
        this.oeBlocktimeApiService
          .$createStrikeGuess(
            accountToken,
            this.strike.blockHeight,
            this.strike.strikeMediantime,
            guess
          )
          .subscribe((slowFastGuess: BlockTimeStrikeGuessPublic) => {
            this.slowFastGuesses = [...this.slowFastGuesses, slowFastGuess];
            this.toastr.success('Guessed successfully!', 'Success!');
          });
      }
    );
    subscription.unsubscribe();
  }

  energyDetailLink() {
    return `/hashstrikes/blockrate-detail/${this.fromBlock.height}/${this.toBlock.height}`;
  }

  strikeSummaryLink() {
    return `/hashstrikes/blockrate-strike-summary?strikeHeight=${this.strike.blockHeight}&strikeTime=${this.strike.strikeMediantime}&blockspanStart=${this.fromBlock.height}`;
  }

  public fetchFutureGuessData(pageNumber: number, filter: any = {}): void {
    this.oeBlocktimeApiService
      .$strikesGuessesWithFilter(filter, pageNumber)
      .subscribe({
        next: (data) => {
          if (!data.results || !Array.isArray(data.results)) {
            this.guessStrike = [];
            return;
          }
          this.guessStrike = data.results;
        },
        error: (error) => this.handleError(error),
      });
  }

  public fetchPastGuessData(pageNumber: number, filter: any = {}): void {
    this.oeBlocktimeApiService
      .$strikesGuessesWithFilter(filter, pageNumber)
      .subscribe({
        next: (data) => {
          if (!data.results || !Array.isArray(data.results)) {
            this.guessStrike = [];
            return;
          }
          this.guessStrike = data.results;
        },
        error: (error) => this.handleError(error),
      });
  }

  private handleError(error: any): void {
    this.toastr.error(`Strikes Failed To Fetch: ${error.error}`, 'Failed!');
  }
}
