import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';

@Component({
  selector: 'app-guessing-block',
  templateUrl: './guessing-block.component.html',
  styleUrls: ['./guessing-block.component.scss'],
})
export class GuessingBlockComponent implements OnInit {
  @Input() type: 'cloud' | 'ice';
  @Input() color = 'red';
  @Input() disabled = false;
  @Output() selectedBlock = new EventEmitter();
  @Input() isSelected = false;

  constructor() {}

  ngOnInit(): void {}

  selectGuessingBox(): void {
    if (!this.disabled) {
      this.isSelected = true;
      this.selectedBlock.emit(this.type === 'cloud' ? 'fast' : 'slow');
    } else {
      // toast
    }
  }
}
