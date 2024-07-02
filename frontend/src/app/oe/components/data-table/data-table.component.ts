import { Component, Input, OnInit, Output, EventEmitter, SimpleChanges } from '@angular/core';
import { TableColumn } from '../../interfaces/oe-energy.interface';

@Component({
  selector: 'app-data-table',
  templateUrl: './data-table.component.html',
  styleUrls: ['./data-table.component.scss'],
})
export class DataTableComponent implements OnInit {
  @Input() headers: TableColumn[] = [];
  @Input() data: any[] = [];
  @Input() isLoading: boolean;
  @Output() rowClicked: EventEmitter<any> = new EventEmitter();
  @Output() getNextPage: EventEmitter<number> = new EventEmitter<number>();
  @Input() currentPage: number = 1;
  @Input() pageSize: number = 100;
  @Input() totalPages: number;
  showSerialNumberColumn: boolean = false;
  startIndex: number = 0;
  pages: number[] = [];

  updatePages(): void {
    const totalVisiblePages = Math.min(this.totalPages, 5);
    const currentPageIndex = this.currentPage - 1;
    const startPage = Math.max(
      0,
      currentPageIndex - Math.floor(totalVisiblePages / 2)
    );

    this.pages = Array.from(
      { length: totalVisiblePages },
      (_, i) => startPage + i + 1
    );
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes.currentPage || changes.totalPages) {
      this.updatePages();
    }
  }

  constructor() {}

  ngOnInit(): void {
    // Determine if the "Sr No" column should be displayed
    this.showSerialNumberColumn = this.headers.some((header) => header.isSrNo);
    this.updatePages();
  }

  onPageChange(pageNumber: number): void {
    this.currentPage = pageNumber;
    this.getNextPage.emit(pageNumber); // Emit the event to fetch data for the new page
  }

  getSerialNumberHeader(): string {
    const srNoColumn = this.headers.find((header) => header.isSrNo);
    return srNoColumn?.customSrNoHeader || 'Sr No';
  }

  onRowClick(item: any): void {
    this.rowClicked.emit(item); // Emit the clicked row's data
  }
}
