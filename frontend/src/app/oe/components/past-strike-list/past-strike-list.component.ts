import {
  MAX_DISPLAYED_STRIKES,
  TABLE_HEADERS,
  URL_PAST_STRIKES_NEWEST_TO_OLDEST,
} from './past-strike-list.interface';
import { Component, OnInit } from '@angular/core';
import { OeBlocktimeApiService } from '../../services/oe-energy.service';
import {
  BlockTimeStrikePast,
  TableColumn,
} from '../../interfaces/oe-energy.interface';
import { ActivatedRoute, Router } from '@angular/router';
import { ToastrService } from 'ngx-toastr';

@Component({
  selector: 'app-past-strike-list',
  templateUrl: './past-strike-list.component.html',
  styleUrls: ['./past-strike-list.component.scss'],
})
export class PastStrikeListComponent implements OnInit {
  reverseOrder: boolean = false;
  isLoading = true;
  headers: TableColumn[] = TABLE_HEADERS;
  pastStrikes: BlockTimeStrikePast[] = [];

  constructor(
    private oeBlocktimeApiService: OeBlocktimeApiService,
    private route: ActivatedRoute,
    private toastr: ToastrService,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.initializeRoute();
    this.fetchPastStrikes();
  }

  private initializeRoute(): void {
    this.route.url.subscribe((url) => {
      const path = url[0].path;
      this.reverseOrder = path === URL_PAST_STRIKES_NEWEST_TO_OLDEST;
    });
  }

  private fetchPastStrikes(): void {
    this.oeBlocktimeApiService.$getPastStrikes('').subscribe({
      next: (data) => this.handleData(data),
      error: (error) => this.handleError(error),
    });
  }

  private handleData(data: BlockTimeStrikePast[]): void {
    this.pastStrikes = (this.reverseOrder ? data.reverse() : data).slice(
      0,
      MAX_DISPLAYED_STRIKES
    );
    this.isLoading = false;
  }

  private handleError(error: any): void {
    this.toastr.error(`Strikes Failed To Fetch: ${error.error}`, 'Failed!');
    this.isLoading = false;
  }

  onChildRowClick(item: BlockTimeStrikePast): void {
    // Construct the URL with parameters from the item
    const url = `/hashstrikes/strike_detail/${item.block}/${item.block + 13}/${
      item.block
    }/${item.observedBlockMediantime}/${item.futureStrikeCreationTime}`;

    // Use the Router service to navigate
    this.router.navigate([url]);
  }
}
