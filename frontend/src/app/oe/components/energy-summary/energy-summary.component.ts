import { Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute, ParamMap, Route, Router } from '@angular/router';
import { switchMap, catchError } from 'rxjs/operators';
import { combineLatest, of, Subscription } from 'rxjs';
import { BlockTypes } from '../../types/constant';
import { Block, TimeStrike } from '../../interfaces/oe-energy.interface';
import { OeEnergyApiService } from '../../services/oe-energy.service';
import { navigator } from '../../utils/helper';

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
  showPreviousBlocklink = true;
  showNextBlocklink = true;
  timeStrikes: TimeStrike[] = [];
  subscription: Subscription;
  blocksSubscription: Subscription;

  constructor(
    private route: ActivatedRoute,
    private oeEnergyApiService: OeEnergyApiService,
    private router: Router,
  ) {}

  ngOnInit() {
    /* this.blocksSubscription = this.stateService.blocks$
      .subscribe(([block]) => {
        this.latestBlock = block;
        this.latestBlocks.unshift(block);
        this.latestBlocks = this.latestBlocks.slice(0, this.stateService.env.KEEP_BLOCKS_AMOUNT);
        this.setNextAndPreviousBlockLink();

        if (block.height === this.fromBlockHeight) {
          this.fromBlock = block;
        }
      }); */

    (this.subscription = this.route.paramMap
      .pipe(
        switchMap((params: ParamMap) => {
          const fromBlockHeight: number = parseInt(params.get('from'), 10);
          const toBlockHeight: number = parseInt(params.get('to'), 10);
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

            return combineLatest([
              this.oeEnergyApiService
                .$getBlockByHeight(fromBlockHeight)
                .pipe(catchError(() => of(fromBlockHeight))),
              this.oeEnergyApiService
                .$getBlockByHeight(toBlockHeight)
                .pipe(catchError(() => of(toBlockHeight))),
            ]);
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
        this.setNextAndPreviousBlockLink();

        this.isLoadingBlock = false;

        /* this.stateService.$accountToken.pipe(take(1)).subscribe(res => {
        this.getTimeStrikes();
      }) */
      })),
      (error) => {
        this.error = error;
        this.isLoadingBlock = false;
      };
  }

  ngOnDestroy() {
    this.subscription.unsubscribe();
  }

  /* getTimeStrikes() {
    this.oeEnergyApiService.$listTimeStrikesByBlockHeight(this.toBlock.height)
      .subscribe((timeStrikes: TimeStrike[]) => {
        this.timeStrikes = timeStrikes.map(strike => ({
          ...strike,
          elapsedTime: strike.nLockTime - this.fromBlock.mediantime
        }));
        // Manually add a strike that is higher energy just to show what happens when it doesn't boil
        const highEnergyStrike = {
          ...this.timeStrikes[0],
          nLockTime: this.toBlock.mediantime - 30
        }
        this.timeStrikes.unshift(highEnergyStrike);
      });
  } */

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

  navigateTo(): void {
    const currentUrl = this.router.url;
    navigator(this.router, currentUrl.replace('energy_summary', 'energy_detail'));
  }
}
