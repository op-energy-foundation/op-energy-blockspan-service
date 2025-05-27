import {
  ChangeDetectionStrategy,
  ChangeDetectorRef,
  Component,
  OnInit,
} from '@angular/core';
import { Router } from '@angular/router';
import {
  OeBlocktimeApiService,
  OeEnergyApiService,
} from '../../services/oe-energy.service';
import {
  Block,
  BlockTimeStrikePublic,
} from '../../interfaces/oe-energy.interface';
import { OeStateService } from '../../services/state.service';
import { of, switchMap, take } from 'rxjs';
import { getNextDifficultyAdjustment } from '../../utils/helper';

@Component({
  selector: 'app-preview',
  templateUrl: './preview.component.html',
  styleUrls: ['./preview.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PreviewComponent implements OnInit {
  latestStrike: BlockTimeStrikePublic;
  isLoading: boolean = true;
  latestBlock: Block;
  epochBlock: Block;

  constructor(
    private router: Router,
    private oeBlocktimeApiService: OeBlocktimeApiService,
    private oeEnergyApiService: OeEnergyApiService,
    public stateService: OeStateService,
    private cdr: ChangeDetectorRef
  ) {}

  fetchLatestStrike(pageNumber: number = 1): void {
    this.oeBlocktimeApiService
      .$outcomeKnownStrikesWithFilter(pageNumber - 1, {
        class: 'guessable',
        linesPerPage: 1,
      })
      .subscribe({
        next: (data) => {
          this.latestStrike = data.results[0];
          this.isLoading = false;
          this.cdr.markForCheck();
        },
        error: (_error) => {
          this.isLoading = false;
          this.cdr.markForCheck();
        },
      });
  }

  fetchEpochBlock(): void {
    const currentEpochIdx = Math.floor(this.latestBlock.height / 2016);
    const currentEpochStart = currentEpochIdx * 2016;
    this.oeEnergyApiService.$getBlockByHeight(currentEpochStart).subscribe({
      next: (data) => {
        this.epochBlock = data;
        this.cdr.markForCheck();
      },
      error: (_error) => {
        this.isLoading = false;
        this.cdr.markForCheck();
      },
    });
  }

  ngOnInit() {
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
        this.fetchEpochBlock();
      });
  }

  blockspansLink(): string {
    return '/hashstrikes/blockspans';
  }

  pastStrikeListOldestToNewestLink(type?: string): string {
    const url = '/hashstrikes/blockrate-strikes-by-blockrate-summary?sort=ascend&page=1&format=table';

    if (type) {
      return `${url}&result=${type}`;
    }
    return url;
  }

  pastStrikeListNewestToOldestLink(type?: string): string {
    const url = '/hashstrikes/blockrate-strikes-by-blockrate-summary?page=1&format=table';

    if (type) {
      return `${url}&result=${type}`;
    }
    return url;
  }

  guessableStrikeListNewestToOldestLink(): string {
    return '/hashstrikes/blockrate-strikes-by-blockrate-summary?outcome=guessable&page=1&format=table';
  }

  pastStrikeListQWithGuess(): string {
    return '/hashstrikes/past_strikes_with_guesses_descending_on_guesses';
  }

  strikesRangeWithStartblockEndblock(): string {
    return '/hashstrikes/blockrate-strikes-by-blockrate-summary?startblock=849237&endblock=849251&page=1&format=table';
  }

  strikesRangeWithStarttimeEndtime(): string {
    return '/hashstrikes/blockrate-strikes-by-blockrate-summary?startTime=1719194635&endTime=1719201779&page=1&format=table';
  }

  strikesRangeWithStartblockEndtime(): string {
    return '/hashstrikes/blockrate-strikes-by-blockrate-summary?startblock=849237&endTime=1719201779&page=1&format=table';
  }

  strikesRangeWithstarttimeEndBlock(): string {
    return '/hashstrikes/blockrate-strikes-by-blockrate-summary?startTime=1719194635&endblock=849251&page=1&format=table';
  }

  strikesRangeWithLastStrikes(): string {
    return '/hashstrikes/blockrate-strikes-by-blockrate-summary?lastStrikes&page=1&format=table';
  }

  strikesRangeWithNextStrikes(): string {
    return '/hashstrikes/blockrate-strikes-by-blockrate-summary?nextStrikes&page=1&format=table';
  }

  strikeListSortByGuess(): string {
    return '/hashstrikes/blockrate-strikes-by-blockrate-summary?sort=descend_guesses_count&page=1&format=table';
  }

  blockspanDetails(): string {
    return `/hashstrikes/blockspan-details?startblock=${
      this.latestBlock?.height
    }&endblock=${this.latestBlock?.height + 2016}`;
  }

  myGuesses(): string {
    return '/hashstrikes/my-guesses';
  }

  blockrateStrikeDetailsV2(
    type: 'past' | 'future' | 'next-difficulty' = 'future'
  ): string {
    if (type === 'past') {
      return `/hashstrikes/blockrate-strike-details?strikeHeight=844447&strikeTime=1716298890&startblock=844433`;
    }

    if (type === 'next-difficulty') {
      const { startBlock, endBlock, strikeTime } = getNextDifficultyAdjustment(
        this.epochBlock
      );
      return `/hashstrikes/blockrate-strike-details?strikeHeight=${endBlock}&strikeTime=${strikeTime}&startblock=${startBlock}`;
    }

    return `/hashstrikes/blockrate-strike-details?strikeHeight=${this.latestStrike?.strike?.block}&strikeTime=${this.latestStrike?.strike?.strikeMediantime}&startblock=${this.latestBlock?.height}`;
  }

  blockrateStrikeSummaryV2(type: 'past' | 'future' = 'future'): string {
    if (type === 'past') {
      return `/hashstrikes/blockrate-strike-summary?strikeHeight=844447&strikeTime=1716298890&startblock=844433`;
    }
    return `/hashstrikes/blockrate-strike-summary?strikeHeight=${this.latestStrike?.strike?.block}&strikeTime=${this.latestStrike?.strike?.strikeMediantime}&startblock=${this.latestBlock?.height}`;
  }

  blockspanSummaryLink(type: 'past' | 'future' = 'future'): string {
    if (type === 'past') {
      return `/hashstrikes/blockrate-summary?startblock=849237&endblock=849251&format=widget`;
    }

    return `/hashstrikes/blockrate-summary?startblock=${this.latestBlock?.height}&endblock=${this.latestStrike?.strike?.block}&format=widget`;
  }

  hashrateSummaryLink(type: 'past' | 'future' = 'future'): string {
    if (type === 'past') {
      return `/hashstrikes/hashrate-summary?startblock=849237&endblock=849251`;
    }
    return `/hashstrikes/hashrate-summary?startblock=${this.latestBlock?.height}&endblock=${this.latestStrike?.strike?.block}`;
  }

  endBlockNWithStrikeSummary(): string {
    return `/hashstrikes/blockrate-summary-endblockmatches?startblock=${this.latestBlock?.height}&endblock=${this.latestStrike?.strike?.block}`;
  }
}
