import {
  Block,
  BlockTimeStrike,
  BlockTimeStrikePublic,
  PaginationResponse,
  TimeStrike,
} from '../../interfaces/oe-energy.interface';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute, ParamMap } from '@angular/router';
import { switchMap, catchError, take } from 'rxjs/operators';
import { combineLatest, of, Subscription } from 'rxjs';
import { BlockTypes } from '../../types/constant';
import {
  OeBlocktimeApiService,
  OeEnergyApiService,
} from '../../services/oe-energy.service';
import { OeStateService } from '../../services/state.service';
import { ToastrService } from 'ngx-toastr';

@Component({
  selector: 'app-strike-summary-with-guess',
  templateUrl: './strike-summary-with-guess.component.html',
  styleUrls: ['./strike-summary-with-guess.component.scss'],
})
export class StrikeSummaryWithGuessComponent implements OnInit, OnDestroy {
  network = '';
  fromBlock: Block;
  toBlock: Block;
  blockHeight: number;
  nextBlockHeight: number;
  fromBlockHash: string;
  strike: BlockTimeStrike;
  toBlockHash: string;
  isLoadingBlock = true;
  latestBlock: Block;
  latestBlocks: Block[] = [];
  error: any;
  subscription: Subscription;
  timeStrikes: BlockTimeStrike[] = [];
  isSelected = false;
  selectedGuess: string;

  get span(): number {
    return this.toBlock.height - this.fromBlock.height;
  }

  get timeDiff(): number {
    return this.toBlock.mediantime - this.fromBlock.mediantime;
  }

  get energyDiff(): number {
    return ((this.span * 600 - this.timeDiff) / (this.span * 600)) * 100;
  }

  get strikeDetailLink(): string {
    return `/hashstrikes/strike_detail?strikeHeight=${this.strike.block}&strikeTime=${this.strike.strikeMediantime}&blockspanStart=${this.fromBlock.height}`;
  }

  constructor(
    private route: ActivatedRoute,
    private oeEnergyApiService: OeEnergyApiService,
    private oeBlocktimeApiService: OeBlocktimeApiService,
    private stateService: OeStateService,
    private toastr: ToastrService
  ) {}

  ngOnInit(): void {
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
            block: strikeHeight,
            strikeMediantime: strikeTime,
            creationTime: undefined,
          };

          // this.fromBlockHeight = fromBlockHeight;
          // this.toBlockHeight = strikeHeight;
          this.blockHeight =
            history.state.data?.blockHeight ?? this.blockHeight;

          document.body.scrollTo(0, 0);

          this.isLoadingBlock = true;

          return combineLatest([
            this.oeEnergyApiService.$getBlockByHeight(fromBlockHeight).pipe(
              catchError(() =>
                of({
                  height: fromBlockHeight,
                  version: 0,
                  merkle_root: '',
                  current_block_hash: '',
                  previous_block_hash: '',
                  timestamp: 0,
                  difficulty: 0,
                  nonce: 0,
                  reward: 0,
                  chainwork: '',
                  mediantime: 0,
                })
              )
            ),
            this.oeEnergyApiService.$getBlockByHeight(strikeHeight).pipe(
              catchError(() =>
                of({
                  height: strikeHeight,
                  version: 0,
                  merkle_root: '',
                  current_block_hash: '',
                  previous_block_hash: '',
                  timestamp: strikeTime,
                  difficulty: 0,
                  nonce: 0,
                  reward: 0,
                  chainwork: '',
                  mediantime: 0,
                })
              )
            ),
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
          this.blockHeight = fromBlock.height;
          this.nextBlockHeight = fromBlock.height + 1;

          const strikesResult = strikesDetails.results;
          if (!strikesResult.length) {
            this.toastr.error('Strikes Not Found!', 'Failed!');
            return;
          }

          this.strike = {
            block: strikesResult[0].strike.block,
            creationTime: strikesResult[0].strike.creationTime,
            strikeMediantime: strikesResult[0].strike.strikeMediantime,
            observedResult: strikesResult[0].strike.observedResult,
          };

          this.isLoadingBlock = false;

          // if(strikesResult[0].strike.observedResult) {
          //   this.toastr.
          // }

          this.checkExistingGuess();
        }
      )),
      (error: Error): void => {
        this.error = error;
        this.isLoadingBlock = false;
      };
  }

  ngOnDestroy(): void {
    this.subscription.unsubscribe();
  }

  handleSelectedGuess(selected: 'slow' | 'fast'): void {
    // this.isSelected = true;
    this.selectedGuess = selected;
    this.oeBlocktimeApiService
      .$strikeGuess(this.strike.block, this.strike.strikeMediantime, selected)
      .subscribe(
        (response) => {
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
            this.selectedGuess = 'disabled';
            this.toastr.warning(
              "Can't add guess as stike outcome is known.",
              'Warning'
            );
          }
          this.isLoadingBlock = false;
        }
      );
  }
}
