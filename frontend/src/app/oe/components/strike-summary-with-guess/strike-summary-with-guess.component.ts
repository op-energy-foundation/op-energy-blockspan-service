import {
  Block,
  BlockSpanTimeStrike,
  PaginationResponse,
} from '../../interfaces/oe-energy.interface';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute, ParamMap } from '@angular/router';
import { switchMap, catchError, take, map } from 'rxjs/operators';
import { combineLatest, of, Subscription } from 'rxjs';
import { BlockTypes } from '../../types/constant';
import { OeEnergyApiService } from '../../services/oe-energy.service';
import { BlockrateTimeStrikeService } from '../../services/blockratetimestrike.service';
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
  strike: BlockSpanTimeStrike;
  toBlockHash: string;
  isLoadingBlock = true;
  latestBlock: Block;
  latestBlocks: Block[] = [];
  error: any;
  subscription: Subscription;
  timeStrikes: BlockSpanTimeStrike[] = [];
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
    return `/hashstrikes/blockrate-strike-detail?strikeHeight=${this.strike.block}&strikeTime=${this.strike.mediantime}&blockspanStart=${this.fromBlock.height}`;
  }

  constructor(
    private route: ActivatedRoute,
    private oeEnergyApiService: OeEnergyApiService,
    private blockrateTimeStrikeService: BlockrateTimeStrikeService,
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
            mediantime: strikeTime,
            creationTime: undefined,
            spanSize: 0,
            guessesCount: 0,
          };

          // this.fromBlockHeight = fromBlockHeight;
          // this.toBlockHeight = strikeHeight;
          this.blockHeight =
            history.state.data?.blockHeight ?? this.blockHeight;

          document.body.scrollTo(0, 0);

          this.isLoadingBlock = true;

          return combineLatest([
            this.oeEnergyApiService
              .$getBlocksByHeights([fromBlockHeight, strikeHeight]),
            this.blockrateTimeStrikeService
              .$strikesWithFilter({
                strikeMediantimeEQ: strikeTime,
                blockHeightEQ: strikeHeight,
              })
              .pipe(catchError(() => of(strikeHeight))),
          ]).pipe(
            map(([blocks, strikes]) => [blocks, strikes] as [Block[], PaginationResponse<BlockSpanTimeStrike> | number])
          );
        })
      )
      .subscribe(
        (result: any) => {
          const [blocks, strikesDetails] = result as [Block[], PaginationResponse<BlockSpanTimeStrike>];
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
          this.blockHeight = fromBlock.height;
          this.nextBlockHeight = fromBlock.height + 1;

          const strikesResult = strikesDetails.results;
          if (!strikesResult.length) {
            this.toastr.error('Strikes Not Found!', 'Failed!');
            return;
          }

          this.strike = {
            block: strikesResult[0].block,
            mediantime: strikesResult[0].mediantime,
            creationTime: strikesResult[0].creationTime,
            spanSize: strikesResult[0].spanSize,
            guessesCount: strikesResult[0].guessesCount,
            observedResult: strikesResult[0].observedResult,
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
    this.blockrateTimeStrikeService
      .$strikeGuess(this.strike.block, this.strike.mediantime, selected)
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
    this.blockrateTimeStrikeService
      .$strikeGuessPerson(this.strike.block, this.strike.mediantime)
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
          }
          this.isLoadingBlock = false;
        }
      );
  }
}
