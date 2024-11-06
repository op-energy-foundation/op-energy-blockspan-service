import { ChangeDetectionStrategy, Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { OeBlocktimeApiService } from '../../services/oe-energy.service';
import {
  Block,
  BlockTimeStrikePublic,
} from '../../interfaces/oe-energy.interface';
import { OeStateService } from '../../services/state.service';
import { of, switchMap, take } from 'rxjs';

@Component({
  selector: 'app-preview',
  templateUrl: './preview.component.html',
  styleUrls: ['./preview.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PreviewComponent implements OnInit {
  latestStrike: BlockTimeStrikePublic;
  isLoading = true;
  latestBlock: Block;

  constructor(
    private router: Router,
    private oeBlocktimeApiService: OeBlocktimeApiService,
    public stateService: OeStateService
  ) {
    this.stateService.latestReceivedBlock$
      .pipe(take(1)) // don't follow any future update of this object
      .pipe(
        switchMap((block: Block) => {
          this.latestBlock = block;
          return of(block);
        })
      )
      .subscribe(() => {
        this.fetchLatestStrike();
      });
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
    return '/hashstrikes/blockrate-strike-summary?strikeHeight=844447&strikeTime=1716298890&blockspanStart=844433';
  }

  futureStrikeSummaryLink(): void {
    window.location.href = `/hashstrikes/blockrate-strike-summary?strikeHeight=${this.latestStrike?.strike?.block}&strikeTime=${this.latestStrike?.strike?.strikeMediantime}`;
  }

  strikeSummaryWithGuess(): void {
    window.location.href = `/hashstrikes/blockrate-strike-summary-with-guess?strikeHeight=${
      this.latestStrike?.strike?.block
    }&strikeTime=${
      this.latestStrike?.strike?.strikeMediantime
    }&blockspanStart=${this.latestStrike?.strike?.block - 14}`;
  }

  pastStrikeDetailLink(): string {
    return '/hashstrikes/blockrate-strike-detail?strikeHeight=844447&strikeTime=1716298890&blockspanStart=844433';
  }

  futureStrikeDetailLink(): void {
    window.location.href = `/hashstrikes/blockrate-strike-detail?strikeHeight=${this.latestStrike?.strike?.block}&strikeTime=${this.latestStrike?.strike?.strikeMediantime}`;
  }

  pastEnergySummaryLink(): string {
    return '/hashstrikes/blockrate-summary?startblock=849237&endblock=849251';
  }

  futureEnergySummaryLink(): void {
    window.location.href = `/hashstrikes/blockrate-summary?endblock=${this.latestStrike?.strike?.block}`;
  }

  pastEnergyDetailLink(): string {
    return '/hashstrikes/blockrate-detail/89778/89791';
  }

  futureEnergyDetailLink(): void {
    window.location.href = `/hashstrikes/blockrate-detail/89778/${this.latestStrike?.strike?.block}`;
  }

  blockRateChartLink(): string {
    return '/graphs/blockratecharts/144';
  }

  pastStrikeListOldestToNewestLink(): string {
    return '/hashstrikes/blockrate-strikes-range?sort=ascend&page=1';
  }

  pastStrikeListNewestToOldestLink(): string {
    return '/hashstrikes/blockrate-strikes-range?page=1';
  }

  guessableStrikeListNewestToOldestLink(): string {
    return '/hashstrikes/blockrate-strikes-range?outcome=guessable&page=1';
  }

  pastStrikeListQWithGuess(): string {
    return '/hashstrikes/past_strikes_with_guesses_descending_on_guesses';
  }

  strikesRangeWithStartblockEndblock(): string {
    return '/hashstrikes/blockrate-strikes-range?startblock=849237&endblock=849251&page=1';
  }

  strikesRangeWithStarttimeEndtime(): string {
    return '/hashstrikes/blockrate-strikes-range?startTime=1719194635&endTime=1719201779&page=1';
  }

  strikesRangeWithStartblockEndtime(): string {
    return '/hashstrikes/blockrate-strikes-range?startblock=849237&endTime=1719201779&page=1';
  }

  strikesRangeWithstarttimeEndBlock(): string {
    return '/hashstrikes/blockrate-strikes-range?startTime=1719194635&endblock=849251&page=1';
  }

  strikesRangeWithLastStrikes(): string {
    return '/hashstrikes/blockrate-strikes-range?lastStrikes&page=1';
  }

  strikesRangeWithNextStrikes(): string {
    return '/hashstrikes/blockrate-strikes-range?nextStrikes&page=1';
  }

  strikeListSortByGuess(): string {
    return '/hashstrikes/blockrate-strikes-range?sort=descend_guesses_count&page=1';
  }

  blockspanDetails(): void {
    window.location.href = `/hashstrikes/blockspan-details?startblock=${
      this.latestBlock?.height
    }&endblock=${this.latestBlock?.height + 2016}`;
  }

  myGuesses(): string {
    return '/hashstrikes/my-guesses';
  }

  blockrateStrikeDetailsV2(): void {
    window.location.href = `/hashstrikes/blockrate-strike-details-v2?strikeHeight=${this.latestStrike?.strike?.block}
    &strikeTime=${this.latestStrike?.strike?.strikeMediantime}&startblock=${this.latestBlock?.height}
    &endblock=${this.latestStrike?.strike?.block}`;
  }
}
