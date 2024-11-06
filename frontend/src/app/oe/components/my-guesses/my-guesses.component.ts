import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { ToastrService } from 'ngx-toastr';
import {
  OeAccountApiService,
  OeBlocktimeApiService,
} from '../../services/oe-energy.service';
import { BlockTimeStrikeGuessPublic } from '../../interfaces/oe-energy.interface';

@Component({
  selector: 'app-my-guesses',
  templateUrl: './my-guesses.component.html',
  styleUrls: ['./my-guesses.component.scss'],
})
export class MyGuessesComponent implements OnInit {
  guessStrike: BlockTimeStrikeGuessPublic[] = [];

  constructor(
    private router: Router,
    private toastr: ToastrService,
    private oeBlocktimeApiService: OeBlocktimeApiService,
    public oeAccountApiService: OeAccountApiService
  ) {}

  ngOnInit(): void {
    this.fetchPastGuessData();
  }

  public fetchPastGuessData(pageNumber: number = 0, filter: any = {}): void {
    const accountUUID = this.oeAccountApiService.$getAccountUUID();
    this.oeBlocktimeApiService
      .$strikesGuessesWithFilter(
        {
          ...filter,
          personEQ: accountUUID,
        },
        pageNumber
      )
      .subscribe({
        next: (data) => {
          if (!data.results || !Array.isArray(data.results)) {
            this.guessStrike = [];
            return;
          }
          this.guessStrike = data.results;
        },
        error: (error) => this.handleError(error),
      });
  }

  private handleError(error: any): void {
    this.toastr.error(`Strikes Failed To Fetch: ${error.error}`, 'Failed!');
  }

  public redirectToSummary(
    event: Event,
    strikeHeight: number,
    strikeTime: number,
    observedResult: string | null
  ): void {

    if (window.getSelection()?.toString()) {
      // If there is selected text, prevent the click event from propagating
      event.stopPropagation();
      return;
    }
    const queryParams: any = {
      strikeHeight: strikeHeight,
      strikeTime: strikeTime,
    };

    // Only add blockspanStart if the result is known (observedResult is not null or empty)
    if (observedResult) {
      queryParams.blockspanStart = strikeHeight - 14; // 14 blocks before the current strike height
    }

    // Navigate to the target route with the query parameters
    this.router.navigate(['/hashstrikes/blockrate-strike-summary'], { queryParams });
  }
}
