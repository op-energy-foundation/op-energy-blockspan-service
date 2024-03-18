import { Component, Input, OnInit } from '@angular/core';
import { TableColumn } from '../../interfaces/oe-energy.interface';

@Component({
  selector: 'app-data-table',
  templateUrl: './data-table.component.html',
  styleUrls: ['./data-table.component.scss']
})
export class DataTableComponent implements OnInit {

  @Input() headers: TableColumn[] = [];
  @Input() data: any[] = [];
  showSerialNumberColumn: boolean = false; // New property

  constructor() { }

  ngOnInit(): void {
    // Determine if the "Sr No" column should be displayed
    this.showSerialNumberColumn = this.headers.some(header => header.isSrNo);
  }

  getSerialNumberHeader(): string {
    const srNoColumn = this.headers.find(header => header.isSrNo);
    return srNoColumn?.customSrNoHeader || 'Sr No';
  }
}
