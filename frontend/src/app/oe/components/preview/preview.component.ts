import { ChangeDetectionStrategy, Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';

@Component({
  selector: 'app-preview',
  templateUrl: './preview.component.html',
  styleUrls: ['./preview.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PreviewComponent implements OnInit {
  constructor(private router: Router) {}

  ngOnInit() {}

  blockspansLink(): string {
    return '/hashstrikes/blockspans/6';
  }

  pastStrikeSummaryLink(): string {
    return '/hashstrikes/strike_summary/89778/89791';
  }

  futureStrikeSummaryLink(): string {
    return '/hashstrikes/strike_summary/89778/1200000';
  }

  pastStrikeDetailLink(): string {
    return '/hashstrikes/strike_detail/89778/89791/89791/1652239330/1656641994';
  }

  futureStrikeDetailLink(): string {
    return '/hashstrikes/strike_detail/89778/1200000/1200000/1652239330/1656641994';
  }

  pastEnergySummaryLink(): string {
    return '/hashstrikes/energy_summary/89778/89791';
  }

  futureEnergySummaryLink(): string {
    return '/hashstrikes/energy_summary/89778/1200000';
  }

  pastEnergyDetailLink(): string {
    return '/hashstrikes/energy_detail/89778/89791';
  }

  futureEnergyDetailLink(): string {
    return '/hashstrikes/energy_detail/89778/1200000';
  }

  blockRateChartLink(): string {
    return '/graphs/blockratecharts/144';
  }
}
