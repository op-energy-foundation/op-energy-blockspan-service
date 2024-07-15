import { TABLE_HEADERS } from './strikes-range.interface';
import { Component, OnInit } from '@angular/core';
import { OeBlocktimeApiService } from '../../services/oe-energy.service';
import {
  BlockTimeStrike,
  BlockTimeStrikePublic,
  PaginationResponse,
  StrikeDetails,
  TableColumn,
} from '../../interfaces/oe-energy.interface';
import { ActivatedRoute, Router } from '@angular/router';
import { ToastrService } from 'ngx-toastr';

@Component({
  selector: 'app-strikes-range',
  templateUrl: './strikes-range.component.html',
  styleUrls: ['./strikes-range.component.scss'],
})
export class StrikesRangeComponent implements OnInit {
  reverseOrder: boolean = false;
  isLoading = true;
  headers: TableColumn[] = TABLE_HEADERS;
  currentPage: number = 1;
  totalPages: number = 1;
  tableData: BlockTimeStrike[] = [];
  filter: any = {};

  constructor(
    private oeBlocktimeApiService: OeBlocktimeApiService,
    private route: ActivatedRoute,
    private toastr: ToastrService,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.route.queryParams.subscribe((params) => {
      this.setFilterFromParams(params);
      this.fetchOutcomeKnownStrikes(this.currentPage);
    });
  }

  setFilterFromParams(params: any): void {
    if (params.startblock) {
      this.filter.strikeBlockHeightGTE = +params.startblock;
    }
    if (params.endblock) {
      this.filter.strikeBlockHeightLTE = +params.endblock;
    }
    if (params.startTime) {
      this.filter.strikeMediantimeGTE = +params.startTime;
    }
    if (params.endTime) {
      this.filter.strikeMediantimeLTE = +params.endTime;
    }
    if (params.sort) {
      this.filter.sort = params.sort;
    }
    if (params.page) {
      this.currentPage = +params.page;
    }
  }

  fetchOutcomeKnownStrikes(pageNumber: number): void {
    this.isLoading = true;
    this.currentPage = pageNumber;
    this.router.navigate([], {
      relativeTo: this.route,
      queryParams: { ...this.filter, page: this.currentPage },
      queryParamsHandling: 'merge',
    });
    this.oeBlocktimeApiService
      .$outcomeKnownStrikesWithFilter(pageNumber - 1, {
        ...this.filter,
        linesPerPage: 15,
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
    this.tableData = data.results.map((result) => result.strike);
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
      blockspanStart: item.block - 13,
    };

    // Use the Router service to navigate with query parameters
    this.router.navigate(['/hashstrikes/strike_detail'], { queryParams });
  }
}
