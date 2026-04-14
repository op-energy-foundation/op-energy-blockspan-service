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
  selector: 'app-strike-summary',
  templateUrl: './strike-summary.component.html',
  styleUrls: ['./strike-summary.component.scss'],
})
export class StrikeSummaryComponent implements OnInit, OnDestroy {
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
  showPreviousBlocklink = true;
  showNextBlocklink = true;
  subscription: Subscription;

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

          if (history.state.data && history.state.data.block) {
            this.blockHeight = history.state.data.block.height;
            return of([history.state.data.block, history.state.data.block]);
          }

          this.isLoadingBlock = true;

          let fromBlockInCache: Block;
          let toBlockInCache: Block;

          fromBlockInCache = this.latestBlocks.find(
            (block: Block) => block.height === fromBlockHeight
          );
          toBlockInCache = this.latestBlocks.find(
            (block: Block) => block.height === strikeHeight
          );
          if (fromBlockInCache && toBlockInCache) {
            return of([fromBlockInCache, toBlockInCache]);
          }
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
          const [blocks, strikesDetails] = result as [Block[], PaginationResponse<BlockSpanTimeStrike> | number];
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

          // Type guard: check if strikesDetails is a PaginationResponse
          if (typeof strikesDetails === 'number') {
            this.toastr.error('Strikes Not Found!', 'Failed!');
            this.isLoadingBlock = false;
            return;
          }

          const strikesResult = strikesDetails.results;
          if (!strikesResult.length) {
            this.toastr.error('Strikes Not Found!', 'Failed!');
            this.isLoadingBlock = false;
            return;
          }

          this.strike = {
            block: strikesResult[0].block,
            mediantime: strikesResult[0].mediantime,
            creationTime: strikesResult[0].creationTime,
            spanSize: strikesResult[0].spanSize,
            guessesCount: strikesResult[0].guessesCount,
          };
          this.setNextAndPreviousBlockLink();

          this.isLoadingBlock = false;
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

  setNextAndPreviousBlockLink(): void {
    if (this.latestBlock && this.blockHeight) {
      this.showPreviousBlocklink = this.blockHeight !== 0;
      this.showNextBlocklink = !(
        this.latestBlock.height && this.latestBlock.height === this.blockHeight
      );
    }
  }
}
