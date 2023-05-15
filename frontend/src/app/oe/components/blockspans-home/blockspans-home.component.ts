import {
  Component,
  OnInit,
  OnDestroy,
  ChangeDetectionStrategy,
  ChangeDetectorRef,
  NgModule,
  HostListener,
  ViewChild,
  ElementRef,
} from '@angular/core';
import { Location } from '@angular/common';
import { forkJoin, Observable, Subscription, of, lastValueFrom } from 'rxjs';
import { ToastrService } from 'ngx-toastr';
import { ActivatedRoute, ParamMap, Router } from '@angular/router';
import { switchMap, take } from 'rxjs/operators';
import { OeStateService } from 'src/app/oe/services/state.service';
import {
  TimeStrike,
  BlockSpan,
} from 'src/app/oe/interfaces/op-energy.interface';
import { Block } from './../../interfaces/op-energy.interface';
import { OpEnergyApiService } from '../../services/oe-energy.service';
import { environment } from '../../../../environments/environment';

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
    private opEnergyApiService: OpEnergyApiService
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
          params.get('tip')
            ? of({ ...params })
            : this.oeStateService.latestReceivedBlock$
                .pipe(take(1)) // don't follow any future update of this object
                .pipe(
                  switchMap((block: Block) =>
                    of({
                      ...params,
                      tip: block.height,
                    })
                  )
                )
        )
      )
      .subscribe((params: any) => {
        const span: string = params.params.span || '';
        const tip: string = params.params.tip || params.tip || '';
        this.blockspanChange({
          tipBlock: +tip,
          span: +span,
        });
      });
  }

  ngOnDestroy() {
    this.subscription.unsubscribe();
  }

  async blockspanChange({ tipBlock, span }): Promise<void> {
    this.span = span;
    const numberOfSpan = this.KEEP_BLOCKS_AMOUNT;
    const blockNumbers = [];
    let blockSpanList = [];
    try {
      blockSpanList = await lastValueFrom(
        this.opEnergyApiService.$getBlockSpanList(
          tipBlock - span * numberOfSpan,
          span,
          numberOfSpan
        ),
        { defaultValue: [] }
      );
    } catch (error) {
      this.toastr.error('Cannot fetch block height data!', 'Failed!');
    }
    blockSpanList.reverse().forEach((blockSpan: BlockSpan) => {
      blockNumbers.push(blockSpan.endBlockHeight);
    });
    blockNumbers.push(blockSpanList[blockSpanList.length - 1].startBlockHeight);
    this.pastBlocks = [];
    forkJoin(
      blockNumbers.map((blockNo) =>
        this.opEnergyApiService.$getBlockByHeight(blockNo)
      )
    )
      .pipe(take(1))
      .subscribe(
        (blocks: any[]) => {
          const updatedBlocks = [];
          for (let index = 0; index < blocks.length - 1; index++) {
            updatedBlocks.push(blocks[index], blocks[index + 1]);
          }
          this.pastBlocks = updatedBlocks;
          this.cd.markForCheck();
          this.lastPastBlock = this.pastBlocks[0];
          this.lastPastBlock = {
            ...this.lastPastBlock,
            height: this.lastPastBlock.height + this.span,
          };
          this.location.replaceState(
            this.router
              .createUrlTree([`/hashstrikes/blockspans/`, this.span, tipBlock])
              .toString()
          );
          this.getTimeStrikes();
        },
        (error) => {
          this.toastr.error('Blockspans are not found!', 'Failed!');
        }
      );
  }

  getTimeStrikes() {
    this.opEnergyApiService
      .$listTimeStrikes()
      .subscribe((timeStrikes: TimeStrike[]) => {
        this.timeStrikes = timeStrikes.map((strike) => ({
          ...strike,
          elapsedTime: strike.nLockTime - this.pastBlocks[0].mediantime,
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
    this.router.navigate([
      '/hashstrikes/blockspan/',
      fromBlock.height,
      toBlock.height,
    ]);
  }

  addStrike(strike) {
    const nLockTime =
      this.pastBlocks[0].mediantime + Number(strike.elapsedTime);
    this.opEnergyApiService
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
