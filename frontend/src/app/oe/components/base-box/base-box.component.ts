import {
  Component,
  OnInit,
  OnDestroy,
  ChangeDetectionStrategy,
  Input,
} from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { navigator, toHHMMSS } from '../../utils/helper';

export const MAX_COUNT = 14;
@Component({
  selector: 'app-base-box',
  templateUrl: './base-box.component.html',
  styleUrls: ['./base-box.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class BaseBoxComponent implements OnInit, OnDestroy {
  @Input() type: 'Energy' | 'Strike' | 'Strike_Boiling' | 'Ice' | 'Cloud' =
    'Energy';
  @Input() color = 'red';
  @Input() totalIconCount: number;
  @Input() fromTime: number;
  @Input() toTime: number;
  @Input() span: number;
  @Input() isUnknown: boolean;
  @Input() isDetailed: boolean;
  @Input() link: string;
  @Input() footerText = 'Time';
  @Input() hideFlameStack = false;
  @Input() customNbdrValue: string;
  @Input() showIcons: boolean = false;
  maxCount = MAX_COUNT;

  get iconArray() {
    const count =
      this.totalIconCount > this.maxCount ? this.maxCount : this.totalIconCount;
    return count ? new Array(count).fill(1) : [];
  }

  get icon() {
    return this.type === 'Energy'
      ? 'fire'
      : this.type === 'Strike'
      ? 'tint'
      : 'cloud';
  }

  get timeSpan() {
    return toHHMMSS(this.toTime - this.fromTime);
  }

  get nbdr() {
    if (this.customNbdrValue) return this.customNbdrValue;

    if (!this.span || !this.toTime || !this.fromTime) return '???';

    return ((600 * 100 * this.span) / (this.toTime - this.fromTime)).toFixed(2);
  }

  constructor(private route: ActivatedRoute, public router: Router) {}

  ngOnInit(): void {}

  ngOnDestroy(): void {}

  navigateTo(): void {
    navigator(this.router, this.link);
  }
}
