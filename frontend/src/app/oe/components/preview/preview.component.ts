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

  pastStrikeListOldestToNewestLink(): string {
    return '/hashstrikes/strikes-range?sort=ascend&page=1';
  }

  pastStrikeListNewestToOldestLink(): string {
    return '/hashstrikes/strikes-range?page=1';
  }

  guessableStrikeListNewestToOldestLink(): string {
    return '/hashstrikes/strikes-range?outcome=guessable&page=1';
  }

  pastStrikeListQWithGuess(): string {
    return '/hashstrikes/past_strikes_with_guesses_descending_on_guesses';
  }

  strikesRangeWithStartblockEndblock(): string {
    return '/hashstrikes/strikes-range?startblock=849237&endblock=849251&page=1';
  }

  strikesRangeWithStarttimeEndtime(): string {
    return '/hashstrikes/strikes-range?startTime=1719194635&endTime=1719201779&page=1';
  }

  strikesRangeWithStartblockEndtime(): string {
    return '/hashstrikes/strikes-range?startblock=849237&endTime=1719201779&page=1';
  }

  strikesRangeWithstarttimeEndBlock(): string {
    return '/hashstrikes/strikes-range?startTime=1719194635&endblock=849251&page=1';
  }

  strikesRangeWithLastStrikes(): string {
    return '/hashstrikes/strikes-range?lastStrikes&page=1';
  }

  strikesRangeWithNextStrikes(): string {
    return '/hashstrikes/strikes-range?nextStrikes&page=1';
  }

  strikeListSortByGuess(): string {
    return '/hashstrikes/strikes-range?sort=descend_guesses_count&page=1';
  }
}
