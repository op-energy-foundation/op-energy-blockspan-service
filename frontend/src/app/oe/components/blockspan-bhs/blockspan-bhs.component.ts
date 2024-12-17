import { Component, ElementRef, Input, OnInit, Renderer2 } from '@angular/core';
import { Block } from '../../interfaces/oe-energy.interface';
import { BlockTypes, Logos } from '../../types/constant';
import { ActivatedRoute, ParamMap } from '@angular/router';
import { OeEnergyApiService } from '../../services/oe-energy.service';
import { OeStateService } from '../../services/state.service';
import { ToastrService } from 'ngx-toastr';
import {
  catchError,
  combineLatest,
  of,
  Subscription,
  switchMap,
  take,
} from 'rxjs';
import {
  getEmptyBlockHeader,
  getHexValue,
  toScientificNotation,
} from '../../utils/helper';

@Component({
  selector: 'app-blockspan-bhs',
  templateUrl: './blockspan-bhs.component.html',
  styleUrls: ['./blockspan-bhs.component.scss'],
})
export class BlockspanBHSComponent implements OnInit {
  logos = Logos;
  isLoadingBlock = true;
  subscription: Subscription;
  fromBlock: Block;
  toBlock: Block;
  latestBlock: Block;
  @Input() result: string;
  addedZeros = false;

  constructor(
    private route: ActivatedRoute,
    private oeEnergyApiService: OeEnergyApiService,
    private stateService: OeStateService,
    private toastr: ToastrService,
    private elementRef: ElementRef
  ) {}

  ngOnInit() {
    (this.subscription = this.route.queryParamMap
      .pipe(
        switchMap((params: ParamMap) =>
          this.stateService.latestReceivedBlock$
            .pipe(take(1)) // don't follow any future update of this object
            .pipe(
              switchMap((block: Block) => {
                this.latestBlock = block;
                return of(params);
              })
            )
        )
      )
      .pipe(
        switchMap((params: ParamMap) => {
          const startBlock = params.get('startblock');
          const endBlock = params.get('strikeHeight') || params.get('endblock');

          if (parseInt(startBlock, 10) > this.latestBlock.height) {
            this.toastr.error('Viewing requires known start block', 'Failed!');
            return of(null);
          }

          if (parseInt(startBlock, 10) >= parseInt(endBlock, 10)) {
            this.toastr.error(
              'Start block must be less than strike height or end block',
              'Failed!'
            );
            return of(null);
          }

          const fromBlockHeight: number =
            endBlock && startBlock
              ? parseInt(startBlock, 10)
              : endBlock
              ? parseInt(endBlock, 10) - 2016
              : parseInt(startBlock, 10) || this.latestBlock.height;

          const toBlockHeight: number =
            parseInt(endBlock, 10) || (startBlock ? +startBlock + 14 : 1200000);

          this.fromBlock = undefined;
          this.toBlock = undefined;

          this.isLoadingBlock = true;

          return combineLatest([
            this.oeEnergyApiService
              .$getBlockByHeight(fromBlockHeight)
              .pipe(catchError(() => of(getEmptyBlockHeader(fromBlockHeight)))),
            this.oeEnergyApiService
              .$getBlockByHeight(toBlockHeight)
              .pipe(catchError(() => of(getEmptyBlockHeader(toBlockHeight)))),
          ]);
        })
      )
      .subscribe(([fromBlock, toBlock]: [Block, Block]) => {
        this.fromBlock = fromBlock;
        this.toBlock = toBlock;
        this.isLoadingBlock = false;
      })),
      (error) => {
        this.toastr.error(`Strikes Failed To Fetch: ${error.error}`, 'Failed!');
        this.isLoadingBlock = false;
      };
  }

  modifyText(text: string): string {
    // Regular expression to match leading zeros
    const regex = /(0+)(.*)$/;
    const match = text.match(regex);
    if (match) {
      const leadingZeros = match[1];
      const remainingText = match[2];

      const groupedZeros = leadingZeros
        .match(/.{1,4}/g) // Break into chunks of 4
        ?.map((group) => (group.length < 4 ? group.padEnd(4, '\u00A0') : group)) // Pad final group with spaces
        .join('\u00A0'); // Join groups with spaces between them

      // Return HTML with leading zeros wrapped in a span
      return `<span class="leading-zeros">${groupedZeros}</span><span class="remaining-text">${remainingText}</span>`;
    }

    // If no leading zeros, return the original text
    return text;
  }

  private processText(selector: string): void {
    const textElement = this.elementRef.nativeElement.querySelector(selector);
    if (textElement) {
      const originalText = textElement.textContent;
      const processedHTML = this.modifyText(originalText);
      // Create a temporary container to parse the HTML
      const tempDiv = document.createElement('div');
      tempDiv.innerHTML = processedHTML;

      // Replace the text node with the new parsed HTML
      while (textElement.firstChild) {
        textElement.removeChild(textElement.firstChild); // Clear existing content
      }

      while (tempDiv.firstChild) {
        textElement.appendChild(tempDiv.firstChild); // Append parsed HTML content
      }
    }
  }

  ngAfterViewChecked(): void {
    if (!this.isLoadingBlock && !this.addedZeros) {
      // making zeroes bold
      this.processText('.hashes .from-block');
      this.processText('.hashes div.to-block');
      this.addedZeros = true;
    }
  }

  convertToUTC(unixTimestamp: number): string {
    if (unixTimestamp === 0) return '?';

    const date = new Date(unixTimestamp * 1000); // Convert seconds to milliseconds

    const year = date.getUTCFullYear();
    const month = String(date.getUTCMonth() + 1).padStart(2, '0'); // Months are 0-based, so add 1
    const day = String(date.getUTCDate()).padStart(2, '0');
    const hours = String(date.getUTCHours()).padStart(2, '0');
    const minutes = String(date.getUTCMinutes()).padStart(2, '0');
    const seconds = String(date.getUTCSeconds()).padStart(2, '0');

    return `${year}-${month}-${day} ${hours}:${minutes}:${seconds}`;
  }

  // Method to calculate the time difference in HH:MM format
  calculateTimeDifference(fromTimestamp: number, toTimestamp: number): string {
    if (fromTimestamp === 0 || toTimestamp === 0) return '?';

    const differenceInSeconds = toTimestamp - fromTimestamp;

    const hours = Math.floor(differenceInSeconds / 3600); // 3600 seconds in an hour
    const minutes = Math.floor((differenceInSeconds % 3600) / 60); // Get remaining minutes

    // Return formatted time difference in HH:MM
    return `${String(hours).padStart(2, '0')}:${String(minutes).padStart(
      2,
      '0'
    )}`;
  }

  getSpan(type: string, isScientific: boolean = true): string {
    if (!this.fromBlock || !this.toBlock) return '?';

    const {
      height: fromHeight,
      mediantime: fromMediantime,
      chainwork: fromChainwork,
      chainreward: fromChainReward,
    } = this.fromBlock;
    const {
      height: toHeight,
      mediantime: toMediantime,
      chainwork: toChainwork,
      chainreward: toChainReward,
    } = this.toBlock;

    switch (type) {
      case 'blockspan':
        return (toHeight - fromHeight).toString();

      case 'time':
        return fromMediantime !== 0 && toMediantime !== 0
          ? (toMediantime - fromMediantime).toString()
          : '?';

      case 'hashes':
        return fromChainwork && toChainwork
          ? isScientific
            ? toScientificNotation(
                getHexValue(toChainwork) - getHexValue(fromChainwork)
              )
            : (getHexValue(toChainwork) - getHexValue(fromChainwork)).toString()
          : '?';

      case 'satoshis':
        return toChainReward && fromChainReward
          ? toScientificNotation(toChainReward - fromChainReward)
          : '?';

      default:
        return '?';
    }
  }

  getBlockRate(): string {
    // Ensure fromBlock and toBlock are valid
    if (!this.fromBlock || !this.toBlock) {
      return '?';
    }

    // Retrieve values from getSpan for 'blockspan' and 'time'
    const blockspan = +this.getSpan('blockspan');
    const time = +this.getSpan('time');

    // Check if the values are valid numbers and time is not zero to avoid NaN or Infinity
    if (isNaN(blockspan) || isNaN(time) || time === 0) {
      return '?'; // Return '?' if the calculation cannot be performed
    }

    // Perform the calculation and ensure it's valid
    return ((600 * 100 * blockspan) / time).toFixed(2);
  }

  getHashRate(): string {
    // Ensure fromBlock and toBlock are valid
    if (!this.fromBlock || !this.toBlock) {
      return '?';
    }

    // Retrieve values from getSpan for 'hashes' and 'time'
    const hashes = +this.getSpan('hashes');
    const time = +this.getSpan('time');

    // Check if the values are valid numbers and time is not zero to avoid NaN or Infinity
    if (isNaN(hashes) || isNaN(time) || time === 0) {
      return '?'; // Return '?' if the calculation cannot be performed
    }

    // Perform the calculation and ensure it's valid
    return toScientificNotation(hashes / time);
  }

  getSathash(): string {
    // Ensure fromBlock and toBlock are valid
    if (!this.fromBlock || !this.toBlock) {
      return '?';
    }

    // Retrieve values from getSpan for 'hashes' and 'satoshis'
    const hashes = +this.getSpan('hashes');
    const satoshis = +this.getSpan('satoshis');

    // Check if the values are valid numbers and satoshis is not zero to avoid NaN or Infinity
    if (isNaN(hashes) || isNaN(satoshis) || satoshis === 0) {
      return '?'; // Return '?' if the calculation cannot be performed
    }

    // Perform the calculation and ensure it's valid
    return toScientificNotation(hashes / satoshis);
  }

  getChainWork(hexValue: string): string {
    // Ensure hexValue are valid
    if (!hexValue) {
      return '?';
    }

    return `${BigInt(`0x${hexValue}`)}`;
  }

  getDifficulty(type: string): string {
    // Ensure fromBlock and toBlock are valid
    if (!type) {
      return '?';
    }

    // Retrieve values from getSpan for 'hashes' and 'satoshis'
    const regex = /(0+)(.*)$/;
    const match = type.match(regex);
    const diffcult = match ? match[1].length : 8;
    return `>= ${this.formatNumberToString(Math.pow(16, diffcult - 8))}`;
  }

  formatNumberToString(input: string | number): string {
    if (!input || input === '?') return '?';

    const number = typeof input === 'string' ? parseFloat(input) : input;
    const formattedNumber = Math.floor(number).toLocaleString('en-US');
    return formattedNumber;
  }
}
