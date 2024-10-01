import { Component, Input, OnInit } from '@angular/core';

@Component({
  selector: 'app-box',
  templateUrl: './box.component.html',
  styleUrls: ['./box.component.scss'],
})
export class BoxComponent implements OnInit {
  @Input() fromLabel: string | number;
  @Input() toLabel: string | number;
  @Input() spanLabel: string;
  @Input() footerText = 'blocks';
  @Input() background: string = 'yellow';
  @Input() logo: string;
  @Input() displayBlock: boolean = true;
  @Input() displayLogo: boolean = true;

  constructor() {}

  ngOnInit(): void {}

  getSpan(): string {
    if (this.spanLabel) return this.spanLabel;

    if (
      typeof this.fromLabel === 'number' &&
      typeof this.toLabel === 'number'
    ) {
      return (this.toLabel - this.fromLabel).toString();
    }

    return '?';
  }
}
