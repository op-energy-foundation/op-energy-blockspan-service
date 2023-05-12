import {
  Component,
  OnInit,
  OnDestroy,
  ChangeDetectionStrategy,
  Input,
} from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { TimeStrike } from 'src/app/oe/interfaces/op-energy.interface';
import { Block } from './../../interfaces/op-energy.interface';

export const MAX_COUNT = 14;

@Component({
  selector: 'app-strike',
  templateUrl: './strike.component.html',
  styleUrls: ['./strike.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class StrikeComponent implements OnInit, OnDestroy {
  @Input() fromBlock: Block;
  @Input() toBlock: Block;
  @Input() strike: TimeStrike;
  maxCount = MAX_COUNT;

  get span(): number {
    return this.toBlock.height - this.fromBlock.height;
  }

  get timeDiff(): number {
    return this.toBlock.mediantime - this.fromBlock.mediantime;
  }

  get isUnknown() {
    return !this.timeDiff;
  }

  get energyDiff(): number {
    return ((this.span * 600 - this.timeDiff) / (this.span * 600)) * 100;
  }

  get totalIconCount() {
    let count = Math.round((6 + this.energyDiff / 5) / 2);
    if (count < 0) {
      count = 0;
    }
    return count;
  }

  get iconArray() {
    const count =
      this.totalIconCount > this.maxCount ? this.maxCount : this.totalIconCount;
    return count ? new Array(count).fill(1) : [];
  }

  get spanWithStrike(): number {
    return this.strike.blockHeight - this.fromBlock.height;
  }

  get timeDiffWithStrike(): number {
    return this.strike.nLockTime - this.fromBlock.mediantime;
  }

  get energyDiffWithStrike(): number {
    return (
      ((this.spanWithStrike * 600 - this.timeDiffWithStrike) /
        (this.spanWithStrike * 600)) *
      100
    );
  }

  get totalIconCountWithStrike() {
    let count = Math.round((6 + this.energyDiffWithStrike / 5) / 2);
    if (count < 0) {
      count = 0;
    }
    return count;
  }

  get strikeType(): 'Energy' | 'Strike' | 'Strike_Boiling' {
    return this.strike.nLockTime > this.toBlock.mediantime
      ? 'Strike_Boiling'
      : 'Strike';
  }

  constructor(private route: ActivatedRoute) {}

  ngOnInit(): void {}

  ngOnDestroy(): void {}

  toHHMMSS(secs) {
    if (!secs) {
      return '??:??:??';
    }
    let sec_num = parseInt(secs, 10); // don't forget the second param
    let hours = Math.floor(sec_num / 3600);
    let minutes = Math.floor((sec_num - hours * 3600) / 60);
    let seconds = sec_num - hours * 3600 - minutes * 60;
    let strHours = hours.toString();
    let strMinutes = minutes.toString();
    let strSeconds = seconds.toString();
    if (hours < 10) {
      strHours = '0' + hours;
    }
    if (minutes < 10) {
      strMinutes = '0' + minutes;
    }
    if (seconds < 10) {
      strSeconds = '0' + seconds;
    }
    return strHours + ':' + strMinutes + ':' + strSeconds;
  }

  strikeDetailLink() {
    return `/hashstrikes/strike_detail/${this.fromBlock.height}/${this.toBlock.height}/${this.strike.blockHeight}/${this.strike.nLockTime}/${this.strike.creationTime}`;
  }
}
