import {
  Component,
  OnInit,
  OnDestroy,
  ChangeDetectionStrategy,
  Input,
  Output,
  EventEmitter,
} from '@angular/core';
import { ActivatedRoute } from '@angular/router';

@Component({
  selector: 'app-blockspan-navigator',
  templateUrl: './blockspan-navigator.component.html',
  styleUrls: ['./blockspan-navigator.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class BlockspanNavigatorComponent implements OnInit, OnDestroy {
  @Input() fromBlock: number;
  @Input() toBlock: number;
  @Output() emitGo = new EventEmitter();

  constructor(private route: ActivatedRoute) {}

  ngOnInit(): void {}

  ngOnDestroy(): void {}

  onGo() {
    this.emitGo.emit({
      tipToBlock: this.toBlock,
      tipFromBlock: this.fromBlock,
    });
  }
}
