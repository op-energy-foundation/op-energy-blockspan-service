import { Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute, ParamMap, Router } from '@angular/router';
import { switchMap, catchError, take } from 'rxjs/operators';
import { combineLatest, of, Subscription } from 'rxjs';
import { APP_CONFIGURATION, BlockTypes } from '../../types/constant';
import {
  Block,
  BlockTimeStrikePublic,
  PaginationResponse,
  StrikesFilter,
  TimeStrike,
} from '../../interfaces/oe-energy.interface';
import {
  OeBlocktimeApiService,
  OeEnergyApiService,
} from '../../services/oe-energy.service';
import { getEmptyBlockHeader, navigator } from '../../utils/helper';
import { OeStateService } from '../../services/state.service';
import { ToastrService } from 'ngx-toastr';

@Component({
  selector: 'app-energy-summary',
  templateUrl: './energy-summary.component.html',
  styleUrls: ['./energy-summary.component.scss'],
})
export class EnergySummaryComponent implements OnInit, OnDestroy {
  network = '';
  fromBlock: Block;
  toBlock: Block | any;
  blockHeight: number;
  nextBlockHeight: number;
  fromBlockHeight: number;
  toBlockHeight: number;
  isLoadingBlock = true;
  latestBlock: Block;
  latestBlocks: Block[] = [];
  error: any;
  paginationMaxSize: number;
  showNextBlocklink = true;
  timeStrikes: TimeStrike[] = [];
  subscription: Subscription;
  blocksSubscription: Subscription;
  filter: StrikesFilter = {};
  strikesData = [] as BlockTimeStrikePublic[];

  constructor(
    private route: ActivatedRoute,
    private oeEnergyApiService: OeEnergyApiService,
    private oeBlocktimeApiService: OeBlocktimeApiService,
    private router: Router,
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
              ? parseInt(endBlock, 10) - APP_CONFIGURATION.SPAN_SIZE
              : parseInt(startBlock, 10) || this.latestBlock.height;

          const toBlockHeight: number =
            parseInt(endBlock, 10) || (startBlock ? +startBlock + 13 : 1200000);

          this.fromBlock = undefined;
          this.toBlock = undefined;
          this.error = undefined;

          if (history.state.data && history.state.data.blockHeight) {
            this.blockHeight = history.state.data.blockHeight;
          }

          this.fromBlockHeight = fromBlockHeight;
          this.toBlockHeight = toBlockHeight;
          document.body.scrollTo(0, 0);

          if (history.state.data && history.state.data.block) {
            this.blockHeight = history.state.data.block.height;
            return of([history.state.data.block, history.state.data.block]);
          } else {
            this.isLoadingBlock = true;

            let fromBlockInCache: Block;
            let toBlockInCache: Block;

            fromBlockInCache = this.latestBlocks.find(
              (block) => block.height === this.fromBlockHeight
            );
            toBlockInCache = this.latestBlocks.find(
              (block) => block.height === this.toBlockHeight
            );
            if (fromBlockInCache && toBlockInCache) {
              return of([fromBlockInCache, toBlockInCache]);
            }

            return this.oeEnergyApiService
              .$getBlocksByHeights([fromBlockHeight, toBlockHeight])
              .pipe(
                catchError(() =>
                  of([
                    getEmptyBlockHeader(fromBlockHeight),
                    getEmptyBlockHeader(toBlockHeight),
                  ])
                )
              );
          }
        })
      )
      .subscribe(([fromBlock, toBlock]: [Block, Block]) => {
        this.fromBlock = fromBlock;
        if (typeof toBlock === BlockTypes.NUMBER) {
          this.toBlock = {
            // TODO: see git log of this piece: it was different from the other components. Is it planned?
            height: +toBlock,
          };
        } else {
          this.toBlock = toBlock;
        }
        this.blockHeight = fromBlock.height;
        this.nextBlockHeight = fromBlock.height + 1;

        //fetching strikes
        this.fetchOutcomeKnownStrikes();
        this.isLoadingBlock = false;
      })),
      (error) => {
        this.error = error;
        this.isLoadingBlock = false;
      };
  }

  ngOnDestroy() {
    this.subscription.unsubscribe();
  }

  fetchOutcomeKnownStrikes(pageNumber: number = 1): void {
    this.isLoadingBlock = true;
    this.oeBlocktimeApiService
      .$outcomeKnownStrikesWithFilter(pageNumber - 1, {
        strikeBlockHeightEQ: this.toBlockHeight,
        linesPerPage: 15,
      })
      .subscribe({
        next: (data) => this.handleData(data),
        error: (error) => this.handleError(error),
      });
  }

  private handleError(error: any): void {
    this.toastr.error(`Strikes Failed To Fetch: ${error.error}`, 'Failed!');
    this.isLoadingBlock = false;
  }

  private handleData(data: PaginationResponse<BlockTimeStrikePublic>): void {
    if (!data.results || !Array.isArray(data.results)) {
      this.strikesData = [];
      this.isLoadingBlock = false;
      return;
    }
    if (data.results.length === 0) {
      this.toastr.warning(
        `Strikes not found please check provided filters`,
        'Warning!'
      );
      this.isLoadingBlock = false;
      return;
    }
    this.strikesData = data.results;
    this.isLoadingBlock = false;
  }

  navigateTo(): void {
    const currentUrl = this.router.url;
    navigator(
      this.router,
      currentUrl.replace('blockrate-summary', 'blockrate-detail')
    );
  }
}
