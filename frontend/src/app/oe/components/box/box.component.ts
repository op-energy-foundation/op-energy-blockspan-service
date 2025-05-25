import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { ClipboardService } from 'ngx-clipboard'

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
  @Input() footerLogo: string;
  @Input() displayBlock: boolean = true;
  @Input() displayLogo: boolean = true;
  @Input() isToolTipEnabled: boolean = false;
  @Output() fromLabelClicked = new EventEmitter<MouseEvent>();
  @Output() toLabelClicked = new EventEmitter<MouseEvent>();

  constructor(private _clipboardService: ClipboardService) {}

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

  copyToClipboard(event: MouseEvent): void {
    const target = event.target as HTMLElement;
    const textToCopy = target.innerText.trim();
    this._clipboardService.copy(textToCopy);

    if (target?.id === 'fromBlockBox' || target?.parentElement?.id === 'fromBlockBox') {
      this.fromLabelClicked.emit(event);
    }
  
    if (target?.id === 'toBlockBox' || target?.parentElement?.id === 'toBlockBox') {
      this.toLabelClicked.emit(event);
    }
  }
}
