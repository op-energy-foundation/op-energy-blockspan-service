import { Component, OnInit } from '@angular/core';
import {
  BlockSpanTimeStrike,
  PaginationResponse,
  TableColumn,
} from '../../interfaces/oe-energy.interface';
import { APP_CONFIGURATION, FormatType, Logos } from '../../types/constant';
import { ActivatedRoute, Router } from '@angular/router';
import { OeEnergyApiService } from '../../services/oe-energy.service';
import { BlockrateTimeStrikeService } from '../../services/blockratetimestrike.service';
import { OeStateService } from '../../services/state.service';
import { ToastrService } from 'ngx-toastr';
import { BaseBlockComponent } from '../common/base-block/BaseBlockComponent';
import { TABLE_HEADERS } from '../strikes-range/strikes-range.interface';

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
  strike: BlockSpanTimeStrike = {} as BlockSpanTimeStrike;
  disabled: boolean = false;
  isSelected: boolean = false;
  selectedGuess: string;
  strikeKnown = false;
  color = 'red';
  strikesData: BlockSpanTimeStrike[] = [];
  tableData: BlockSpanTimeStrike[] = [];
  headers: TableColumn[] = TABLE_HEADERS;
  

  constructor(
    router: Router,
    private route: ActivatedRoute,
    oeEnergyApiService: OeEnergyApiService,
    stateService: OeStateService,
    blockrateTimeStrikeService: BlockrateTimeStrikeService,
    toastr: ToastrService
  ) {
    super(
      router,
      oeEnergyApiService,
      blockrateTimeStrikeService,
      stateService,
      toastr
    );
  }

  async ngOnInit() {
    // Initialize the latest block
    await this.initializeLatestBlock();

    // Process query parameters
    const params = this.processQueryParams(this.route);

    this.format = params.format;

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

  ngOnDestroy(): void {
    // Call the cleanup method from the base class
    this.cleanup();
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

    return this.strike.observedBlockMediantime > this.strike.mediantime;
  }

  fetchOutcomeNotKnownStrikes(pageNumber: number = 1): void {
    this.isLoadingBlock = true;
    this.blockrateTimeStrikeService
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

  private handleData(data: PaginationResponse<BlockSpanTimeStrike>): void {
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
    if (this.format === FormatType.TABLE) {
      this.tableData = data.results.map((result) => {
        return {
          ...result,
          guessesCount: result.guessesCount,
        };
      });
    } else {
      this.strikesData = data.results;
    }
    this.isLoadingBlock = false;
  }

  onChildRowClick(item: BlockSpanTimeStrike): void {
    // Construct the query parameters
    const queryParams = {
      strikeHeight: item.block,
      strikeTime: item.mediantime,
      startblock: item.block - APP_CONFIGURATION.SPAN_SIZE,
    };

    // Use the Router service to navigate with query parameters
    this.router.navigate(['/hashstrikes/blockrate-strike-details'], { queryParams });
  }
}
