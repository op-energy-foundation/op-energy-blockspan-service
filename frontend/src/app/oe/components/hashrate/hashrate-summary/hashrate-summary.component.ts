import { Component, OnInit } from '@angular/core';
import {
  BlockTimeStrike,
  BlockTimeStrikePublic,
} from '../../../interfaces/oe-energy.interface';
import { GENESIS_K, Logos } from '../../../types/constant';
import { ActivatedRoute, Router } from '@angular/router';
import {
  OeBlocktimeApiService,
  OeEnergyApiService,
} from '../../../services/oe-energy.service';
import { OeStateService } from '../../../services/state.service';
import { ToastrService } from 'ngx-toastr';
import { BaseBlockComponent } from '../../common/base-block/BaseBlockComponent';

@Component({
  selector: 'app-hashrate-summary',
  templateUrl: './hashrate-summary.component.html',
  styleUrls: ['./hashrate-summary.component.scss']
})
export class HashrateSummaryComponent  extends BaseBlockComponent implements OnInit {

  logos = Logos;
    strike: BlockTimeStrike = {} as BlockTimeStrike;
    disabled: boolean = false;
    isSelected: boolean = false;
    selectedGuess: string;
    strikeKnown = false;
    color = 'red';
    strikesData = [] as BlockTimeStrikePublic[];
  
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
      // Initialize the latest block
      await this.initializeLatestBlock();
  
      // Process query parameters
      const params = this.processQueryParams(this.route);
  
      // Extract values from processed query parameters
      const { fromBlockHeight, toBlockHeight } = this.calculateBlockRange({
        startBlock: +params.startblock,
        endBlock: +params.endblock,
      });
  
      // Initialize blocks using the base class method
      this.subscription = this.initializeBlocks(
        fromBlockHeight,
        toBlockHeight
      ).subscribe(
        ([fromBlock, toBlock]: any) => {
          this.fromBlock = fromBlock;
          this.toBlock = toBlock;
          this.isLoadingBlock = false;
        },
        (error) => {
          this.handleError(error);
          this.isLoadingBlock = false;
        }
      );
    }
  
    ngOnDestroy(): void {
      // Call the cleanup method from the base class
      this.cleanup();
    }

    
    getHeight(): string {
      const no_adjustment_hashrate = GENESIS_K  * (this.fromBlock?.difficulty || 0);
      const hashrateString = this.getHashRate();

      if(hashrateString === '?') {
        return '1';
      }

      const hashrate = Number(hashrateString);
      return (hashrate / (hashrate + no_adjustment_hashrate)).toFixed(2);
    }
}
