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
    return '/hashstrikes/strike_summary?strikeHeight=844447&strikeTime=1716298890&blockspanStart=844433';
  }

  futureStrikeSummaryLink(): string {
    return '/hashstrikes/strike_summary?strikeHeight=1200000';
  }

  strikeSummaryWithGuess(): string {
    return '/hashstrikes/strike_summary_with_guess?strikeHeight=852329&strikeTime=1721063768&blockspanStart=852316';
  }

  pastStrikeDetailLink(): string {
    return '/hashstrikes/strike_detail?strikeHeight=844447&strikeTime=1716298890&blockspanStart=844433';
  }

  futureStrikeDetailLink(): string {
    return '/hashstrikes/strike_detail?strikeHeight=1200000';
  }

  pastEnergySummaryLink(): string {
    return '/hashstrikes/energy_summary?startblock=849237&endblock=849251';
  }

  futureEnergySummaryLink(): string {
    return '/hashstrikes/energy_summary';
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

  pastStrikeListOldestToNewestLink(): string {
    return '/hashstrikes/strikes-range?sort=ascend';
  }

  pastStrikeListNewestToOldestLink(): string {
    return '/hashstrikes/strikes-range';
  }

  guessableStrikeListNewestToOldestLink(): string {
    return '/hashstrikes/strikes-range?outcome=guessable';
  }

  pastStrikeListQWithGuess(): string {
    return '/hashstrikes/past_strikes_with_guesses_descending_on_guesses';
  }

  strikesRangeWithStartblockEndblock(): string {
    return '/hashstrikes/strikes-range?startblock=849237&endblock=849251';
  }

  strikesRangeWithStarttimeEndtime(): string {
    return '/hashstrikes/strikes-range?startTime=1719194635&endTime=1719201779';
  }

  strikesRangeWithStartblockEndtime(): string {
    return '/hashstrikes/strikes-range?startblock=849237&endTime=1719201779';
  }

  strikesRangeWithstarttimeEndBlock(): string {
    return '/hashstrikes/strikes-range?startTime=1719194635&endblock=849251';
  }
}
