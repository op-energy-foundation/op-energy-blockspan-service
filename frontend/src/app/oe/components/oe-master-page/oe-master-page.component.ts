import { Component } from '@angular/core';
import { OeStateService } from 'src/app/oe/services/state.service';

@Component({
  selector: 'app-oe-master-page',
  templateUrl: './oe-master-page.component.html',
  styleUrls: ['./oe-master-page.component.scss'],
})
export class OeMasterPageComponent {
  navCollapsed = false;

  constructor(public readonly oeStateService: OeStateService) {}

  collapse(): void {
    this.navCollapsed = !this.navCollapsed;
  }
}
