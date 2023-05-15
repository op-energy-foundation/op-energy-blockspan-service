import { Component } from '@angular/core';

@Component({
  selector: 'app-oe-master-page',
  templateUrl: './oe-master-page.component.html',
  styleUrls: ['./oe-master-page.component.scss'],
})
export class OeMasterPageComponent {
  navCollapsed = false;
  
  collapse(): void {
    this.navCollapsed = !this.navCollapsed;
  }
}
