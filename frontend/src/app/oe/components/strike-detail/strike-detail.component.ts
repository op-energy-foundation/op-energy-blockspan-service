import { Block } from '../../interfaces/oe-energy.interface';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { Location } from '@angular/common';
import { ActivatedRoute, ParamMap, Router } from '@angular/router';
import { Subscription, of, combineLatest } from 'rxjs';
import { switchMap, catchError, map, take } from 'rxjs/operators';
import { NgbModal } from '@ng-bootstrap/ng-bootstrap';
import { ToastrService } from 'ngx-toastr';
import {
  SlowFastGuess,
  TimeStrike,
} from 'src/app/oe/interfaces/oe-energy.interface';
import { BlockTypes } from '../../types/constant';
import { OeEnergyApiService } from '../../services/oe-energy.service';
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
  slowFastGuesses: SlowFastGuess[] = [];
  currentActiveGuess: 'slow' | 'fast' | null = null;

  get strikeElapsedTime(): number {
    return this.strike.nLockTime - this.fromBlock.mediantime;
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
    return this.strike.nLockTime - this.fromBlock.mediantime;
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
    return this.strike.nLockTime > this.toBlock.mediantime
      ? 'Strike_Boiling'
      : 'Strike';
  }

  constructor(
    private route: ActivatedRoute,
    private location: Location,
    private router: Router,
    private modalService: NgbModal,
    private toastr: ToastrService,
    private oeEnergyApiService: OeEnergyApiService,
    public stateService: OeStateService
  ) {}

  ngOnInit() {
    /* this.blocksSubscription = this.stateService.blocks$.subscribe(([block]) => {
      this.latestBlock = block;
      this.latestBlocks.unshift(block);
      this.latestBlocks = this.latestBlocks.slice(
        0,
        this.stateService.env.KEEP_BLOCKS_AMOUNT
      );
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
          this.strike = {
            blockHeight: +params.get('strikeBlockHeight'),
            nLockTime: +params.get('strikeMedianTime'),
            creationTime: +params.get('strikeCreationTime'),
          };
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
            height: +toBlock,
          };
        } else {
          this.toBlock = toBlock;
        }
        this.blockHeight = fromBlock.height;
        this.nextBlockHeight = fromBlock.height + 1;
        this.setNextAndPreviousBlockLink();

        this.isLoadingBlock = false;

        /*  this.stateService.$accountToken.pipe(take(1)).subscribe((res) => {
          this.getGuesses();
        }); */
      })),
      (error) => {
        this.error = error;
        this.isLoadingBlock = false;
      };
  }

  ngOnDestroy() {
    this.subscription.unsubscribe();
  }

  getGuesses() {
    this.oeEnergyApiService
      .$listSlowFastGuesses(this.strike)
      .subscribe((slowFastGuess: SlowFastGuess[]) => {
        this.slowFastGuesses = slowFastGuess;
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
    this.oeEnergyApiService
      .$slowFastGuess(guess, this.strike)
      .subscribe((slowFastGuess: SlowFastGuess) => {
        this.slowFastGuesses = [...this.slowFastGuesses, slowFastGuess];
        this.toastr.success('Guessed successfully!', 'Success!');
      });
  }

  energyDetailLink() {
    return `/hashstrikes/energy_detail/${this.fromBlock.height}/${this.toBlock.height}`;
  }

  strikeDetailLink() {
    return `/hashstrikes/strike_detail/${this.fromBlock.height}/${this.toBlock.height}/${this.strike.blockHeight}/${this.strike.nLockTime}/${this.strike.creationTime}`;
  }
}
