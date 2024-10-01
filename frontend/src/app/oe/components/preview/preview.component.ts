import { ChangeDetectionStrategy, Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { OeBlocktimeApiService } from '../../services/oe-energy.service';
import { BlockTimeStrikePublic } from '../../interfaces/oe-energy.interface';

@Component({
  selector: 'app-preview',
  templateUrl: './preview.component.html',
  styleUrls: ['./preview.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PreviewComponent implements OnInit {
  latestStrike: BlockTimeStrikePublic;
  isLoading = true;
  constructor(
    private router: Router,
    private oeBlocktimeApiService: OeBlocktimeApiService
  ) {
    this.fetchLatestStrike();
  }

  fetchLatestStrike(pageNumber: number = 1): void {
    this.isLoading = true;
    this.oeBlocktimeApiService
      .$outcomeKnownStrikesWithFilter(pageNumber - 1, {
        class: 'guessable',
        linesPerPage: 1,
      })
      .subscribe({
        next: (data) => {
          this.latestStrike = data.results[0];
          this.isLoading = false;
        },
        error: (_error) => {
          this.isLoading = false;
        },
      });
  }

  ngOnInit() {}

  blockspansLink(): string {
    return '/hashstrikes/blockspans';
  }

  pastStrikeSummaryLink(): string {
    return '/hashstrikes/strike_summary?strikeHeight=844447&strikeTime=1716298890&blockspanStart=844433';
  }

  futureStrikeSummaryLink(): void {
    window.location.href = `/hashstrikes/strike_summary?strikeHeight=${this.latestStrike?.strike?.block}&strikeTime=${this.latestStrike?.strike?.strikeMediantime}`;
  }

  strikeSummaryWithGuess(): void {
    window.location.href = `/hashstrikes/strike_summary_with_guess?strikeHeight=${
      this.latestStrike?.strike?.block
    }&strikeTime=${
      this.latestStrike?.strike?.strikeMediantime
    }&blockspanStart=${this.latestStrike?.strike?.block - 14}`;
  }

  pastStrikeDetailLink(): string {
    return '/hashstrikes/strike_detail?strikeHeight=844447&strikeTime=1716298890&blockspanStart=844433';
  }

  futureStrikeDetailLink(): void {
    window.location.href = `/hashstrikes/strike_detail?strikeHeight=${this.latestStrike?.strike?.block}&strikeTime=${this.latestStrike?.strike?.strikeMediantime}`;
  }

  pastEnergySummaryLink(): string {
    return '/hashstrikes/energy_summary?startblock=849237&endblock=849251';
  }

  futureEnergySummaryLink(): void {
    window.location.href = `/hashstrikes/energy_summary?endblock=${this.latestStrike?.strike?.block}`;
  }

  pastEnergyDetailLink(): string {
    return '/hashstrikes/energy_detail/89778/89791';
  }

  futureEnergyDetailLink(): void {
    window.location.href = `/hashstrikes/energy_detail/89778/${this.latestStrike?.strike?.block}`;
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

  blockspanDetails(): string {
    return `/hashstrikes/blockspan-details?endblock=${this.latestStrike?.strike?.block}`;
  }
  
  myGuesses(): string {
    return '/hashstrikes/my_guesses';
  }
}
