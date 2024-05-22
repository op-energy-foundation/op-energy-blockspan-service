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
    return '/hashstrikes/blockspans';
  }

  pastStrikeSummaryLink(): string {
    return '/hashstrikes/strike_summary/89778/89791';
  }

  futureStrikeSummaryLink(): string {
    return '/hashstrikes/strike_summary/89778/1200000';
  }

  pastStrikeDetailLink(): string {
    return '/hashstrikes/strike_detail?strikeHeight=844447&strikeTime=1716298890&blockspanStart=844433';
  }

  futureStrikeDetailLink(): string {
    return '/hashstrikes/strike_detail?strikeHeight=1200000';
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
