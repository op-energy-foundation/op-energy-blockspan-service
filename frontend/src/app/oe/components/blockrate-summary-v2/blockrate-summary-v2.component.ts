import { Component, OnInit } from '@angular/core';
import {
  BlockTimeStrike,
  BlockTimeStrikePublic,
  PaginationResponse,
} from '../../interfaces/oe-energy.interface';
import { Logos } from '../../types/constant';
import { ActivatedRoute, Router } from '@angular/router';
import {
  OeBlocktimeApiService,
  OeEnergyApiService,
} from '../../services/oe-energy.service';
import { OeStateService } from '../../services/state.service';
import { ToastrService } from 'ngx-toastr';
import { BaseBlockComponent } from '../common/base-block/BaseBlockComponent';

@Component({
  selector: 'app-blockrate-summary-v2',
  templateUrl: './blockrate-summary-v2.component.html',
  styleUrls: ['./blockrate-summary-v2.component.scss'],
})
export class BlockrateSummaryV2Component
  extends BaseBlockComponent
  implements OnInit
{
  logos = Logos;
  strike: BlockTimeStrike = {} as BlockTimeStrike;
  disabled: boolean = false;
  isSelected: boolean = false;
  selectedGuess: string;
  strikeKnown = false;
  color = 'red';
  strikesData = [] as BlockTimeStrikePublic[];

  constructor(
    router: Router,
    private route: ActivatedRoute,
    oeEnergyApiService: OeEnergyApiService,
    stateService: OeStateService,
    oeBlocktimeApiService: OeBlocktimeApiService,
    toastr: ToastrService
  ) {
    super(
      router,
      oeEnergyApiService,
      oeBlocktimeApiService,
      stateService,
      toastr
    );
  }

  async ngOnInit() {
    // Initialize the latest block
    await this.initializeLatestBlock();

    // Process query parameters
    const params = this.processQueryParams(this.route);

    // Extract values from processed query parameters
    const { fromBlockHeight, toBlockHeight } = this.calculateBlockRange({
      startBlock: +params.startblock,
      endBlock: +params.endblock,
    });

    // Initialize blocks using the base class method
    this.subscription = this.initializeBlocks(
      fromBlockHeight,
      toBlockHeight
    ).subscribe(
      ([fromBlock, toBlock]: any) => {
        this.fromBlock = fromBlock;
        this.toBlock = toBlock;
        this.isLoadingBlock = false;
        this.fetchOutcomeNotKnownStrikes();
      },
      (error) => {
        this.handleError(error);
        this.isLoadingBlock = false;
      }
    );
  }

  getResult(): string {
    if (!this.strike.observedBlockHeight) return;

    const heightOverStrikeHeight = this.getJudgementHeight();
    const timeOverStrikeTime = this.getJudgementTime();

    if (heightOverStrikeHeight && !timeOverStrikeTime) return 'fast';

    if (!heightOverStrikeHeight && timeOverStrikeTime) return 'slow';

    return 'slow';
  }

  getJudgementHeight(): boolean {
    if (!this.strike.observedBlockHeight) {
      return;
    }

    return this.strike.observedBlockHeight > this.strike.block - 1;
  }

  getJudgementTime(): boolean {
    if (!this.strike.observedBlockMediantime) {
      return;
    }

    return this.strike.observedBlockMediantime > this.strike.strikeMediantime;
  }

  fetchOutcomeNotKnownStrikes(pageNumber: number = 1): void {
    this.isLoadingBlock = true;
    this.oeBlocktimeApiService
      .$outcomeKnownStrikesWithFilter(pageNumber - 1, {
        strikeBlockHeightEQ: this.toBlock.height,
        linesPerPage: 15,
      })
      .subscribe({
        next: (data) => this.handleData(data),
        error: (error) =>
          this.handleError(`Strikes Failed To Fetch: ${error.error}`),
      });
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
}
