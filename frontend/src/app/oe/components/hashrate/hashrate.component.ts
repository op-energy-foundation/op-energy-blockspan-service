import { Component, OnInit } from '@angular/core';
import {
  Block,
  BlockTimeStrike,
  BlockTimeStrikePublic,
} from '../../interfaces/oe-energy.interface';
import { BlockTypes, Logos } from '../../types/constant';
import { ActivatedRoute, ParamMap, Router } from '@angular/router';
import {
  OeBlocktimeApiService,
  OeEnergyApiService,
} from '../../services/oe-energy.service';
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
  selector: 'app-hashrate',
  templateUrl: './hashrate.component.html',
  styleUrls: ['./hashrate.component.scss'],
})
export class HashrateComponent implements OnInit {
  logos = Logos;
  isLoadingBlock = true;
  subscription: Subscription;
  strike: BlockTimeStrike = {} as BlockTimeStrike;
  fromBlock: Block;
  toBlock: Block;
  latestBlock: Block;
  disabled: boolean = false;
  isSelected: boolean = false;
  selectedGuess: string;
  strikeKnown = false;
  color = 'red';
  strikesData = [] as BlockTimeStrikePublic[];

  constructor(
    private router: Router,
    private route: ActivatedRoute,
    private oeEnergyApiService: OeEnergyApiService,
    private stateService: OeStateService,
    private oeBlocktimeApiService: OeBlocktimeApiService,
    private toastr: ToastrService
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
          const startBlock = params.get('startblock') as string;
          const endBlock = params.get('endblock') as string;

          if (parseInt(startBlock, 10) > this.latestBlock.height) {
            this.toastr.error('Viewing requires known start block', 'Failed!');
            return of(null);
          }

          if (parseInt(startBlock, 10) >= parseInt(endBlock, 10)) {
            this.toastr.error(
              'Start block must be less than end block',
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
            parseInt(endBlock, 10) ||
            (startBlock ? +startBlock + 2016 : 1200000);

          // this.fromBlock = undefined;
          // this.toBlock = undefined;

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
        if (typeof toBlock === BlockTypes.NUMBER) {
          this.toBlock = {
            ...this.fromBlock,
            height: +toBlock,
          };
        } else {
          this.toBlock = toBlock;
        }
        this.isLoadingBlock = false;
      })),
      (error) => {
        this.toastr.error(
          `Blockspan Failed To Fetch: ${error.error}`,
          'Failed!'
        );
        this.isLoadingBlock = false;
      };
  }

  getSpan(type: string): string {
    if (!this.fromBlock || !this.toBlock) return '?';

    if (type === 'blockspan') {
      return (this.toBlock.height - this.fromBlock.height).toString();
    }

    if (type === 'time') {
      return !this.fromBlock.mediantime || !this.toBlock.mediantime
        ? '?'
        : (this.toBlock.mediantime - this.fromBlock.mediantime).toString();
    }

    if (type === 'hashes') {
      return toScientificNotation(
        getHexValue(this.toBlock.chainwork) -
          getHexValue(this.fromBlock.chainwork)
      );
    }

    if (type === 'satoshis') {
      return '?';
    }

    return '?';
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

  goToStrikeDetails(event: Event): string {
    // TODO:
    return '';
  }
}
