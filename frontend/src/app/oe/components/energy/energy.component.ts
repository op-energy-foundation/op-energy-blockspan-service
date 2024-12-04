import {
  Component,
  OnInit,
  OnDestroy,
  ChangeDetectionStrategy,
  Input,
} from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Block } from '../../interfaces/oe-energy.interface';

@Component({
  selector: 'app-energy',
  templateUrl: './energy.component.html',
  styleUrls: ['./energy.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class EnergyComponent implements OnInit, OnDestroy {
  @Input() fromBlock: Block;
  @Input() toBlock: Block;
  @Input() isDetailed: boolean;
  @Input() hideFlameStack = false;

  get span(): number {
    return this.toBlock.height - this.fromBlock.height;
  }

  get timeDiff(): number {
    return this.toBlock.mediantime - this.fromBlock.mediantime;
  }

  get energyDiff(): number {
    return ((this.span * 600 - this.timeDiff) / (this.span * 600)) * 100;
  }

  get totalIconCount(): number {
    let count = Math.round((6 + this.energyDiff / 5) / 2);
    if (count < 0) {
      count = 0;
    }
    return count;
  }

  constructor(private route: ActivatedRoute) {}

  ngOnInit(): void {}

  ngOnDestroy(): void {}

  energyDetailLink(): string {
    return `/hashstrikes/blockspan-details?startblock=${this.fromBlock.height}&endblock=${this.toBlock.height}`;
  }
}
