import {
  Component,
  OnInit,
  OnDestroy,
  ChangeDetectionStrategy,
  ChangeDetectorRef,
  ViewChild,
  ElementRef,
} from '@angular/core';
import { Location } from '@angular/common';
import { Subscription, from, of } from 'rxjs';
import { ToastrService } from 'ngx-toastr';
import { ActivatedRoute, ParamMap, Router } from '@angular/router';
import { switchMap, take } from 'rxjs/operators';
import { OeStateService } from 'src/app/oe/services/state.service';
import { TimeStrike } from 'src/app/oe/interfaces/oe-energy.interface';
import {
  BlockHeader,
  BlockSpanHeaders,
} from './../../interfaces/oe-energy.interface';
import { Block } from '../../interfaces/oe-energy.interface';
import { OeEnergyApiService } from '../../services/oe-energy.service';
import { environment } from '../../../../environments/environment';
import { getBlockSpanByHeight } from '../../utils/helper';

interface PastBlock extends Block {
  mediantimeDiff: number;
}

@Component({
  selector: 'app-blockspans-home',
  templateUrl: './blockspans-home.component.html',
  styleUrls: ['./blockspans-home.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class BlockspansHomeComponent implements OnInit, OnDestroy {
  KEEP_BLOCKS_AMOUNT = environment.KEEP_BLOCKS_AMOUNT;
  allBlocks: PastBlock[] = [];
  pastBlocks: PastBlock[] = [];
  indexArray = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21];
  lastPastBlock: PastBlock;
  emptyBlocks: Block[] = this.mountEmptyBlocks();
  markHeight: number;
  subscription: Subscription;
  blockStyles = [];
  emptyBlockStyles = [];
  tabHidden = false;
  arrowVisible = false;
  arrowLeftPx = 30;
  blocksFilled = false;
  transition = '1s';

  span = 1;

  gradientColors = {
    '': ['#9339f4', '#105fb0'],
  };

  mouseDragStartX: number;
  blockchainScrollLeftInit: number;
  @ViewChild('blockchainContainer') blockchainContainer: ElementRef;

  timeStrikes: TimeStrike[] = [];
  initStrike = 1200;

  constructor(
    private location: Location,
    private toastr: ToastrService,
    public stateService: OeStateService,
    private oeStateService: OeStateService,
    private route: ActivatedRoute,
    private router: Router,
    private cd: ChangeDetectorRef,
    private oeEnergyApiService: OeEnergyApiService
  ) {}

  ngOnInit() {
    this.emptyBlocks.forEach((b) =>
      this.emptyBlockStyles.push(this.getStyleForEmptyBlock(b))
    );

    for (let i = 0; i < this.indexArray.length; i++) {
      this.blockStyles.push(this.getStyleForBlock(i));
    }

    this.subscription = this.route.paramMap
      .pipe(
        switchMap((params: ParamMap) =>
          params.get('from') && params.get('from') !== 'undefined'
            ? of({
                from: params.get('from'),
                to: params.get('to'),
              })
            : this.oeStateService.latestReceivedBlock$
                .pipe(take(1)) // don't follow any future update of this object
                .pipe(
                  switchMap((block: Block) =>
                    of({
                      from: block.height - 14,
                      to: block.height,
                    })
                  )
                )
        )
      )
      .subscribe((params: any) => {
        const fromBlock: string = params.from || '';
        const toBlock: string = params.to || '';
        this.blockspanChange({
          tipFromBlock: fromBlock,
          tipToBlock: toBlock,
        });
      });
  }

  ngOnDestroy() {
    this.subscription.unsubscribe();
  }

  async blockspanChange({ tipFromBlock, tipToBlock }): Promise<void> {
    const numberOfSpan = this.KEEP_BLOCKS_AMOUNT;
    this.span = tipToBlock - tipFromBlock;
    const { fromBlock, toBlock } = getBlockSpanByHeight(
      tipToBlock,
      numberOfSpan,
      this.span
    );
    this.pastBlocks = [];
    this.oeEnergyApiService
      .$getBlocksByBlockSpan(fromBlock, this.span, numberOfSpan)
      .pipe(take(1))
      .pipe(
        switchMap((blockHeaders: BlockSpanHeaders[]) => {
          const acc: BlockHeader[] = [];
          blockHeaders.reverse().forEach(({ endBlock, startBlock }) => {
            return acc.push(endBlock, startBlock);
          });
          return [acc];
        })
      )
      .subscribe(
        (blocks: any[]) => {
          this.pastBlocks = blocks;
          this.cd.markForCheck();
          this.lastPastBlock = this.pastBlocks[0];
          this.lastPastBlock = {
            ...this.lastPastBlock,
            mediantime: null,
            height: this.lastPastBlock.height + this.span,
          };
          this.location.replaceState(
            this.router
              .createUrlTree([
                `/hashstrikes/blockspans/`,
                tipFromBlock,
                tipToBlock,
              ])
              .toString()
          );
          this.getTimeStrikes();
        },
        (error) => {
          this.toastr.error(
            error.error || 'Blockspans are not found!',
            'Failed!'
          );
        }
      );
  }

  getTimeStrikes() {
    this.oeEnergyApiService
      .$listTimeStrikes()
      .subscribe((timeStrikes: TimeStrike[]) => {
        this.timeStrikes = timeStrikes.map((strike) => ({
          ...strike,
          elapsedTime: strike.strikeMediantime - this.pastBlocks[0].mediantime,
        }));
        const existingElapsedTimes = this.timeStrikes.map((s) => s.elapsedTime);
        while (existingElapsedTimes.includes(this.initStrike)) {
          this.initStrike += 1;
        }
      });
  }

  onMouseDown(event: MouseEvent) {
    this.mouseDragStartX = event.clientX;
    this.blockchainScrollLeftInit =
      this.blockchainContainer.nativeElement.scrollLeft;
  }
  onDragStart(event: MouseEvent) {
    // Ignore Firefox annoying default drag behavior
    event.preventDefault();
  }

  trackByBlocksFn(index: number, item: Block) {
    return item.height;
  }
  trackByPastBlocksFn(index: number, item: PastBlock) {
    return item.height;
  }

  getStyleForBlock(index: number) {
    return {
      left: 250 + 295 * (index + 1) + 'px',
    };
  }

  getStyleForEmptyBlock(block: Block) {
    let addLeft = 0;

    if (block.stage === 1) {
      block.stage = 2;
      addLeft = -205;
    }

    return {
      left: addLeft + 195 * this.emptyBlocks.indexOf(block) + 'px',
      background: '#2d3348',
    };
  }

  mountEmptyBlocks() {
    const emptyBlocks = [];
    for (let i = 0; i < this.KEEP_BLOCKS_AMOUNT; i++) {
      emptyBlocks.push({
        id: '',
        height: 0,
        version: 0,
        timestamp: 0,
        bits: 0,
        nonce: 0,
        difficulty: 0,
        merkle_root: '',
        tx_count: 0,
        size: 0,
        weight: 0,
        previousblockhash: '',
        matchRate: 0,
        stage: 0,
      });
    }
    return emptyBlocks;
  }

  goDetail(fromBlock, toBlock) {
    this.router.navigate(['/hashstrikes/blockspan-details'], {
      queryParams: {
        startblock: fromBlock.height,
        endblock: toBlock.height,
      },
    });
  }

  addStrike(strike) {
    const nLockTime =
      this.pastBlocks[0].mediantime + Number(strike.elapsedTime);
    this.oeEnergyApiService
      .$addTimeStrike(strike.blockHeight, nLockTime)
      .subscribe(
        (timeStrike) => {
          this.getTimeStrikes();
          this.toastr.success(
            'A strike has been added successfully!',
            'Success!'
          );
        },
        (err) => {
          this.toastr.error('Error occurred!', 'Failed!');
        }
      );
  }
}
