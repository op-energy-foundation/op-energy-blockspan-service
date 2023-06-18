import { Block } from '../../interfaces/oe-energy.interface';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { Location } from '@angular/common';
import { ActivatedRoute, ParamMap, Router } from '@angular/router';
import { switchMap, catchError, take } from 'rxjs/operators';
import { combineLatest, of, Subscription } from 'rxjs';
import { NgbModal } from '@ng-bootstrap/ng-bootstrap';
import {
  TimeStrike,
  NavigationObject,
} from 'src/app/oe/interfaces/oe-energy.interface';
import { BlockTypes } from '../../types/constant';
import { OeEnergyApiService } from '../../services/oe-energy.service';
import { OeStateService } from '../../services/state.service';

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
  toBlockHash: string;
  isLoadingBlock = true;
  latestBlock: Block;
  latestBlocks: Block[] = [];
  error: any;
  showPreviousBlocklink = true;
  showNextBlocklink = true;
  isSelected = false;
  subscription: Subscription;
  selectedGuess: string;
  
  timeStrikes: TimeStrike[] = [];

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
    return `/hashstrikes/strike_detail/${this.fromBlock.height}/${this.toBlock.height}/${this.toBlock.height}/${this.fromBlock.mediantime}/${this.toBlock.mediantime}`;
  }

  constructor(
    private route: ActivatedRoute,
    private location: Location,
    private router: Router,
    private modalService: NgbModal,
    private oeEnergyApiService: OeEnergyApiService,
    private stateService: OeStateService
  ) {}

  ngOnInit(): void {
    (this.subscription = this.route.paramMap
      .pipe(
        switchMap((params: ParamMap) => {
          const fromBlockHeight: number = 792288;
          const toBlockHeight: number = 794304;
          this.fromBlock = null;
          this.toBlock = null;
          this.error = null;

          if (history.state.data && history.state.data.blockHeight) {
            this.blockHeight = history.state.data.blockHeight;
          }

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
            (block: Block) => block.height === toBlockHeight
          );
          if (fromBlockInCache && toBlockInCache) {
            return of([fromBlockInCache, toBlockInCache]);
          }
          return combineLatest([
            this.oeEnergyApiService.$getBlockByHeight(fromBlockHeight),
            this.oeEnergyApiService
              .$getBlockByHeight(toBlockHeight)
              .pipe(catchError(() => of(toBlockHeight))),
          ]);
        })
      )
      .subscribe(([fromBlock, toBlock]: [Block, Block]) => {
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
        this.setNextAndPreviousBlockLink();

        this.isLoadingBlock = false;

        this.stateService.$accountToken.pipe(take(1)).subscribe(() => {
          this.getTimeStrikes();
        });
      })),
      (error: Error): void => {
        this.error = error;
        this.isLoadingBlock = false;
      };
  }

  ngOnDestroy(): void {
    this.subscription.unsubscribe();
  }

  getTimeStrikes(): void {
    this.oeEnergyApiService
      .$listTimeStrikesByBlockHeight(this.toBlock.height)
      .subscribe((timeStrikes: TimeStrike[]) => {
        this.timeStrikes = timeStrikes.map((strike) => ({
          ...strike,
          elapsedTime: strike.nLockTime - this.fromBlock.mediantime,
        }));
        // Manually add a strike that is higher energy just to show what happens when it doesn't boil
        const highEnergyStrike = {
          ...this.timeStrikes[0],
          nLockTime: this.toBlock.mediantime - 30,
        };
        this.timeStrikes.unshift(highEnergyStrike);
      });
  }

  getNavigationObject(block: Block, nextBlockHeight: number): NavigationObject {
    return { state: { data: { block, blockHeight: nextBlockHeight } } };
  }

  setNextAndPreviousBlockLink(): void {
    if (this.latestBlock && this.blockHeight) {
      this.showPreviousBlocklink = this.blockHeight !== 0;
      this.showNextBlocklink = !(
        this.latestBlock.height && this.latestBlock.height === this.blockHeight
      );
    }
  }

  open(content): void {
    this.modalService.open(content, { ariaLabelledBy: 'modal-basic-title' })
      .result;
  }

  goDetail(fromBlock, strike): void {
    this.router.navigate([
      '/hashstrikes/strike_detail/',
      fromBlock.height,
      strike.blockHeight,
      strike.blockHeight,
      strike.nLockTime,
      strike.creationTime,
    ]);
  }

  handleSelectedGuess(selected: any) {
    this.isSelected = true;
    console.log(selected);
    this.selectedGuess = selected
  }
}
