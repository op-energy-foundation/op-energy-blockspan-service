import { Component, OnInit } from '@angular/core';
import {
  BlockTimeStrike,
  BlockTimeStrikePublic,
  PaginationResponse,
} from '../../interfaces/oe-energy.interface';
import { Logos } from '../../types/constant';
import { ActivatedRoute, Router } from '@angular/router';
import {
  OeBlocktimeApiService,
  OeEnergyApiService,
} from '../../services/oe-energy.service';
import { OeStateService } from '../../services/state.service';
import { ToastrService } from 'ngx-toastr';
import { BaseBlockComponent } from '../common/base-block/BaseBlockComponent';

@Component({
  selector: 'app-block-rate-strike-details-v2',
  templateUrl: './block-rate-strike-details-v2.component.html',
  styleUrls: ['./block-rate-strike-details-v2.component.scss'],
})
export class BlockRateStrikeDetailsV2Component
  extends BaseBlockComponent
  implements OnInit
{
  logos = Logos;
  isLoadingBlock = true;
  disabled: boolean = false;
  isSelected: boolean = false;
  selectedGuess: string;
  strikeKnown = false;
  color = 'red';

  constructor(
    router: Router,
    private route: ActivatedRoute,
    oeEnergyApiService: OeEnergyApiService,
    stateService: OeStateService,
    oeBlocktimeApiService: OeBlocktimeApiService,
    toastr: ToastrService
  ) {
    super(
      router,
      oeEnergyApiService,
      oeBlocktimeApiService,
      stateService,
      toastr
    );
  }

  async ngOnInit() {
    await this.initializeLatestBlock();
    const params = this.processQueryParams(this.route);

    const {
      fromBlockHeight,
      toBlockHeight: strikeHeight,
      strikeTime,
    } = this.calculateBlockRange({
      startBlock: +params.startblock,
      endBlock: +params.strikeHeight,
      strikeTime: +params.strikeTime,
    });

    this.subscription = this.initializeBlocks(
      fromBlockHeight,
      strikeHeight,
      strikeTime
    ).subscribe(
      ([fromBlock, toBlock, strikesDetails]: [
        any,
        any,
        PaginationResponse<BlockTimeStrikePublic>
      ]) => {
        this.fromBlock = fromBlock;
        this.toBlock = toBlock;

        if (!strikesDetails.results.length) {
          this.toastr.error('Strikes Not Found!', 'Failed!');
          return;
        }

        this.strike = {
          ...strikesDetails.results[0].strike,
          block: strikesDetails.results[0].strike.block,
          creationTime: strikesDetails.results[0].strike.creationTime,
          strikeMediantime: strikesDetails.results[0].strike.strikeMediantime,
          observedResult: strikesDetails.results[0].strike.observedResult,
        };

        // debugger;

        console.log(this.getSpan('time'));
        this.isLoadingBlock = false;
        this.checkExistingGuess();
      },
      (error) => {
        this.handleError(`Strikes Failed To Fetch: ${error.error}`);
        this.isLoadingBlock = false;
      }
    );
  }

  getResult(): string {
    if (!this.strike.observedBlockHeight) return;

    const heightOverStrikeHeight = this.getJudgementHeight();
    const timeOverStrikeTime = this.getJudgementTime();

    if (heightOverStrikeHeight && !timeOverStrikeTime) return 'fast';

    if (!heightOverStrikeHeight && timeOverStrikeTime) return 'slow';

    return 'slow';
  }

  getJudgementResult(): string {
    const result = this.getResult();

    if (result === 'fast') {
      return 'faster';
    }

    if (result === 'slow') {
      return 'slower';
    }

    return '?';
  }

  handleSelectedGuess(selected: 'slow' | 'fast'): void {
    // this.isSelected = true;
    this.selectedGuess = selected;
    this.oeBlocktimeApiService
      .$strikeGuess(this.strike.block, this.strike.strikeMediantime, selected)
      .subscribe(
        (response) => {
          this.disabled = true;
          this.toastr.success('Successfully added guess', 'Success');
        },
        (error) => {
          this.toastr.error(
            'Failed to add guess. Error: ' + error.error,
            'Failed!'
          );
        }
      );
  }

  checkExistingGuess(): void {
    this.isLoadingBlock = true;
    this.oeBlocktimeApiService
      .$strikeGuessPerson(this.strike.block, this.strike.strikeMediantime)
      .subscribe(
        (response) => {
          this.isLoadingBlock = false;
          this.disabled = true;
          this.toastr.warning(
            'You already have a guess: ' + response.guess,
            'Warning'
          );
          this.selectedGuess = response.guess;
        },
        (error) => {
          // DOING NOTHING
          if (this.strike.observedResult) {
            //disabling strike as strike outcome is known
            this.disabled = true;
            this.strikeKnown = true;
            this.toastr.warning(
              "Can't add guess as stike outcome is known.",
              'Warning'
            );
          }
          this.isLoadingBlock = false;
        }
      );
  }

  getJudgementHeight(): boolean {
    if (!this.strike.observedBlockHeight) {
      return;
    }

    return this.strike.observedBlockHeight > this.strike.block - 1;
  }

  getJudgementTime(): boolean {
    if (!this.strike.observedBlockMediantime) {
      return;
    }

    return this.strike.observedBlockMediantime > this.strike.strikeMediantime;
  }
}
