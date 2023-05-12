import {
  Component,
  OnInit,
  OnDestroy,
  ChangeDetectionStrategy,
  Input,
} from '@angular/core';
import { ActivatedRoute } from '@angular/router';

export const MAX_COUNT = 14;
@Component({
  selector: 'app-base-box-v2',
  templateUrl: './base-box-v2.component.html',
  styleUrls: ['./base-box-v2.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class BaseBoxV2Component implements OnInit, OnDestroy {
  @Input() type: 'Energy' | 'Strike' | 'Strike_Boiling' = 'Energy';
  @Input() color = 'red';
  @Input() fromTime: number;
  @Input() toTime: number;
  @Input() span: number;
  @Input() link: string;
  maxCount = MAX_COUNT;

  get icon() {
    return this.type === 'Energy'
      ? 'fire'
      : this.type === 'Strike'
      ? 'tint'
      : 'cloud';
  }

  get timeDiff(): number {
    return this.toTime - this.fromTime;
  }

  get timeSpan() {
    return this.toHHMMSS(this.timeDiff);
  }

  get nbdr(): string {
    if (!this.span || !this.timeDiff) return '???';
    return ((600 * 100 * this.span) / this.timeDiff).toFixed(2);
  }

  constructor(private route: ActivatedRoute) {}

  ngOnInit(): void {}

  ngOnDestroy(): void {}

  toHHMMSS(secs) {
    if (!(secs > 0)) {
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
}
