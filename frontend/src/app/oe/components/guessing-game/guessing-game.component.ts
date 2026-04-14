import { Component, Input, OnInit, SimpleChanges } from '@angular/core';
import { Block, BlockSpanTimeStrike } from '../../interfaces/oe-energy.interface';
import { BlockrateTimeStrikeService } from '../../services/blockratetimestrike.service';
import { ToastrService } from 'ngx-toastr';

@Component({
  selector: 'app-guessing-game',
  templateUrl: './guessing-game.component.html',
  styleUrls: ['./guessing-game.component.scss'],
})
export class GuessingGameComponent implements OnInit {
  @Input() fromBlock: Block;
  @Input() toBlock: Block;
  @Input() strike: BlockSpanTimeStrike;
  selectedGuess: string;
  isLoadingBlock = false;
  strikeKnown: boolean = false;

  constructor(
    private blockrateTimeStrikeService: BlockrateTimeStrikeService,
    private toastr: ToastrService
  ) {}

  ngOnInit(): void {
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes.strike && changes.strike.currentValue) {
      this.checkExistingGuess();
    }
  }

  checkExistingGuess(): void {
    if (!this.strike) {
      return;
    }

    this.isLoadingBlock = true;
    this.blockrateTimeStrikeService
      .$strikeGuessPerson(this.strike.block, this.strike.mediantime)
      .subscribe(
        (response) => {
          this.isLoadingBlock = false;
          this.toastr.warning(
            'You already have a guess: ' + response.guess,
            'Warning'
          );
          this.selectedGuess = response.guess;
        },
        (error) => {
          if (this.strike.observedResult) {
            this.selectedGuess = 'disabled';
            this.strikeKnown = true;
          }
          this.isLoadingBlock = false;
        }
      );
  }

  handleSelectedGuess(selected: 'slow' | 'fast'): void {
    this.selectedGuess = selected;
    this.blockrateTimeStrikeService
      .$strikeGuess(this.strike.block, this.strike.mediantime, selected)
      .subscribe(
        (response) => {
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

  get span(): number {
    return this.toBlock.height - this.fromBlock.height;
  }

  get timeDiff(): number {
    return this.toBlock.mediantime - this.fromBlock.mediantime;
  }

  get energyDiff(): number {
    return ((this.span * 600 - this.timeDiff) / (this.span * 600)) * 100;
  }
}
