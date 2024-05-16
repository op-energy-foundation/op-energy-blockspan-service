import { Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute, ParamMap, Router } from '@angular/router';
import { switchMap, catchError, take } from 'rxjs/operators';
import { combineLatest, Observable, of, Subscription } from 'rxjs';
import {
  Block,
  EnergyNbdrStatistics,
  TimeStrike,
} from 'src/app/oe/interfaces/oe-energy.interface';
import { ToastrService } from 'ngx-toastr';
import { OeEnergyApiService } from '../../services/oe-energy.service';
import { BlockTypes } from '../../types/constant';
import { OeStateService } from '../../services/state.service';
import { navigator } from '../../utils/helper';

@Component({
  selector: 'app-energy-detail',
  templateUrl: './energy-detail.component.html',
  styleUrls: ['./energy-detail.component.scss'],
})
export class EnergyDetailComponent implements OnInit, OnDestroy {
  network = '';
  fromBlock: Block;
  // Used any to populate just the block height property
  toBlock: Block | any;
  blockHeight: number;
  nextBlockHeight: number;
  fromBlockHash: string;
  toBlockHash: string;
  isLoadingBlock = true;
  latestBlock: Block;
  latestBlocks: Block[] = [];
  isLoadingTransactions = true;
  error: any;
  paginationMaxSize: number;
  page = 1;
  txsLoadingStatus$: Observable<number>;
  showPreviousBlocklink = true;
  showNextBlocklink = true;

  subscription: Subscription;
  keyNavigationSubscription: Subscription;
  blocksSubscription: Subscription;
  networkChangedSubscription: Subscription;

  timeStrikes: TimeStrike[] = [];
  showStrikes: boolean;

  average: string = null;
  stddev: string = null;

  get span(): number {
    return this.toBlock.height - this.fromBlock.height;
  }

  get timeDiff(): number {
    return this.toBlock.mediantime - this.fromBlock.mediantime;
  }

  get energyDiff(): number {
    return ((this.span * 600 - this.timeDiff) / (this.span * 600)) * 100;
  }

  constructor(
    private route: ActivatedRoute,
    private oeEnergyApiService: OeEnergyApiService,
    private toastr: ToastrService,
    private stateService: OeStateService,
    public router: Router
  ) {}

  ngOnInit() {
    this.paginationMaxSize = window.matchMedia('(max-width: 670px)').matches
      ? 3
      : 5;

    (this.subscription = this.route.paramMap
      .pipe(
        switchMap((params: ParamMap) => {
          const fromBlockHeight: number = parseInt(params.get('from'), 10);
          const toBlockHeight: number = parseInt(params.get('to'), 10);
          this.fromBlock = undefined;
          this.toBlock = undefined;
          this.page = 1;
          this.error = undefined;

          if (history.state.data && history.state.data.blockHeight) {
            this.blockHeight = history.state.data.blockHeight;
          }

          document.body.scrollTo(0, 0);

          if (history.state.data && history.state.data.block) {
            this.blockHeight = history.state.data.block.height;
            return of([history.state.data.block, history.state.data.block]);
          } else {
            this.isLoadingBlock = true;

            let fromBlockInCache: Block = this.latestBlocks.find(
              (block) => block.height === fromBlockHeight
            );
            let toBlockInCache: Block = this.latestBlocks.find(
              (block) => block.height === toBlockHeight
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
            height: +toBlock,
          };
        } else {
          this.toBlock = toBlock;
        }
        this.blockHeight = fromBlock.height;
        this.nextBlockHeight = fromBlock.height + 1;
        this.setNextAndPreviousBlockLink();

        /*
        this.oeEnergyApiService
          .$getNbdrStatistics(fromBlock.height - this.span * 100, this.span)
          .subscribe({
            next: (data: EnergyNbdrStatistics) => {
              this.average = data.nbdr.avg.toFixed(2);
              this.stddev = data.nbdr.stddev.toFixed(2);
            },
            error: (error) => {
              this.toastr.error('Unable to fetch Nbdr Statistics!', 'Failed!');
            },
          });
        */
        this.isLoadingBlock = false;
        this.isLoadingTransactions = true;

        this.stateService.$accountToken.pipe(take(1)).subscribe((res) => {
          this.getTimeStrikes();
        });
      })),
      (error) => {
        this.error = error;
        this.isLoadingBlock = false;
      };
  }

  ngOnDestroy() {
    this.subscription.unsubscribe();
  }

  getTimeStrikes() {
    this.oeEnergyApiService
      .$listTimeStrikesByBlockHeight(this.toBlock.height)
      .subscribe((timeStrikes: TimeStrike[]) => {
        this.timeStrikes = timeStrikes.map((strike) => ({
          ...strike,
          elapsedTime: strike.nLockTime - this.fromBlock.mediantime,
        }));
      });
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

  navigateTo(): void {
    const currentUrl = this.router.url;
    navigator(this.router, currentUrl.replace('energy_detail', 'energy_summary'));
  }

  openExternalSite(event: Event, hash: string | undefined): void {
    event.stopPropagation(); // Stop the event propagation
    if (hash) {
      // Use window.open or any other method to open the external site
      window.open('https://blockstream.info/block/' + hash, '_blank');
    }
  }
}
