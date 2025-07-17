import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-judgement-block',
  templateUrl: './judgement-block.component.html',
  styleUrls: ['./judgement-block.component.scss']
})
export class JudgementBlockComponent {
  @Input() strike: any;

  getJudgementHeight(): boolean | undefined {
    if (!this.strike?.observedBlockHeight) return;
    return this.strike.observedBlockHeight > this.strike.block - 1;
  }

  getJudgementTime(): boolean | undefined {
    if (!this.strike?.observedBlockMediantime) return;
    return this.strike.observedBlockMediantime > this.strike.strikeMediantime;
  }

   getResult(): string {
    if (!this.strike.observedBlockHeight) return;

    const heightOverStrikeHeight = this.getJudgementHeight();
    const timeOverStrikeTime = this.getJudgementTime();

    if (heightOverStrikeHeight && !timeOverStrikeTime) return 'fast';

    if (!heightOverStrikeHeight && timeOverStrikeTime) return 'slow';

    return 'slow';
  }

}
