import {
  Component,
  OnInit,
  OnDestroy,
  ChangeDetectionStrategy,
  Input,
} from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Block } from '../../interfaces/oe-energy.interface';
import { navigator } from '../../utils/helper';

@Component({
  selector: 'app-blockspan',
  templateUrl: './blockspan.component.html',
  styleUrls: ['./blockspan.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class BlockspanComponent implements OnInit, OnDestroy {
  @Input() fromBlock: Block;
  @Input() toBlock: Block;
  @Input() footerText = 'Blocks';

  get span(): number {
    return this.toBlock.height - this.fromBlock.height;
  }

  get fromDetailLink() {
    return `/hashstrikes/block/${this.fromBlock.height}`;
  }

  get toDetailLink() {
    return `/hashstrikes/block/${this.toBlock.height}`;
  }

  constructor(private route: ActivatedRoute, public router: Router) {}

  ngOnInit(): void {}

  ngOnDestroy(): void {}

  navigateTo(link: string): void {
    navigator(this.router, link);
  }
}
