import { TABLE_HEADERS } from './strikes-range.interface';
import { Component, OnInit } from '@angular/core';
import { OeBlocktimeApiService } from '../../services/oe-energy.service';
import {
  Block,
  BlockTimeStrike,
  BlockTimeStrikePublic,
  PaginationResponse,
  StrikeDetails,
  StrikesFilter,
  TableColumn,
} from '../../interfaces/oe-energy.interface';
import { ActivatedRoute, Router } from '@angular/router';
import { ToastrService } from 'ngx-toastr';
import { Subscription, take } from 'rxjs';
import { OeStateService } from '../../services/state.service';
import { APP_CONFIGURATION } from '../../types/constant';

@Component({
  selector: 'app-strikes-range',
  templateUrl: './strikes-range.component.html',
  styleUrls: ['./strikes-range.component.scss'],
})
export class StrikesRangeComponent implements OnInit {
  private blockSubscription: Subscription;
  private paramsSubscription: Subscription;
  reverseOrder: boolean = false;
  isLoading = true;
  headers: TableColumn[] = TABLE_HEADERS;
  currentPage: number = 1;
  totalPages: number = 1;
  tableData: BlockTimeStrike[] = [];
  filter: any = {
    class: 'outcomeKnown',
  }; // This will be used for API calls
  urlFilter: StrikesFilter = {}; // This will be used for URL parameters
  guessableScreen = false;
  paramMappings = {
    startblock: 'strikeBlockHeightGTE',
    endblock: 'strikeBlockHeightLTE',
    startTime: 'strikeMediantimeGTE',
    endTime: 'strikeMediantimeLTE',
    sort: 'sort',
    page: 'page',
    outcome: 'class',
    result: 'observedResultEQ',
  };
  currentTip: number;
  linesPerPage = 15;

  constructor(
    private oeBlocktimeApiService: OeBlocktimeApiService,
    private stateService: OeStateService,
    private route: ActivatedRoute,
    private toastr: ToastrService,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.blockSubscription = this.stateService.latestReceivedBlock$
      .pipe(take(1)) // take only the latest block once
      .subscribe((block: Block) => {
        this.currentTip = block.height;

        this.paramsSubscription = this.route.queryParams.subscribe((params) => {
          this.setFilterFromParams(params);
          this.fetchOutcomeKnownStrikes(this.currentPage);
        });
      });
  }

  ngOnDestroy(): void {
    if (this.blockSubscription) {
      this.blockSubscription.unsubscribe();
    }
    if (this.paramsSubscription) {
      this.paramsSubscription.unsubscribe();
    }
  }

  setFilterFromParams(params: any): void {
    Object.keys(this.paramMappings).forEach((key) => {
      if (params[key]) {
        this.filter[this.paramMappings[key]] = +params[key] || params[key];
        this.urlFilter[key] = params[key];
        if (key === 'page') {
          this.currentPage = +params[key];
        }
        if (key === 'outcome') {
          this.guessableScreen = true;
        }
      }
    });
    if (params.hasOwnProperty('nextStrikes')) {
      delete this.filter.class;
      this.filter.strikeBlockHeightGTE = this.currentTip;
      this.filter.strikeBlockHeightLTE = this.currentTip + this.linesPerPage;
    }
    if (params.hasOwnProperty('lastStrikes')) {
      this.filter.strikeBlockHeightGTE = this.currentTip - this.linesPerPage;
      this.filter.strikeBlockHeightLTE = this.currentTip;
    }
    if (params['sort'] === 'descend_guesses_count') {
      delete this.filter.class;
    }
  }

  fetchOutcomeKnownStrikes(pageNumber: number): void {
    this.isLoading = true;
    this.currentPage = pageNumber;
    this.router.navigate([], {
      relativeTo: this.route,
      queryParams: { ...this.urlFilter, page: this.currentPage },
      queryParamsHandling: 'merge',
    });
    this.oeBlocktimeApiService
      .$outcomeKnownStrikesWithFilter(pageNumber - 1, {
        ...this.filter,
        linesPerPage: this.linesPerPage,
      })
      .subscribe({
        next: (data) => this.handleData(data),
        error: (error) => this.handleError(error),
      });
  }

  private handleData(data: PaginationResponse<BlockTimeStrikePublic>): void {
    if (!data.results || !Array.isArray(data.results)) {
      this.tableData = [];
      this.isLoading = false;
      return;
    }
    if (data.results.length === 0) {
      this.toastr.warning(
        `Strikes not found please check provided filters`,
        'Warning!'
      );
      this.isLoading = false;
      return;
    }
    this.tableData = data.results.map((result) => {
      const strike = result.strike;
      const queryParams = {
        strikeHeight: strike.block,
        strikeTime: strike.strikeMediantime,
        startblock: Math.min(this.currentTip, strike.block - APP_CONFIGURATION.SPAN_SIZE),
      };
      return {
        ...strike,
        guessesCount: result.guessesCount,
        queryParams, // Add queryParams for use in data-table
        routerLink: '/hashstrikes/blockrate-strike-summary', // Add routerLink for use in data-table
      };
    });
    // this.totalPages = Math.floor(data.count / data.results.length);
    this.isLoading = false;
  }

  private handleError(error: any): void {
    this.toastr.error(`Strikes Failed To Fetch: ${error.error}`, 'Failed!');
    this.isLoading = false;
  }

  onChildRowClick(item: StrikeDetails): void {
    // Construct the query parameters
    const queryParams = {
      strikeHeight: item.block,
      strikeTime: item.strikeMediantime,
      startblock: Math.min(this.currentTip, item.block - APP_CONFIGURATION.SPAN_SIZE),
    };

    // all pages should goes to strike summary
    // Use the Router service to navigate with query parameters
    this.router.navigate(['/hashstrikes/blockrate-strike-summary'], {
      queryParams,
    });
  }
}
