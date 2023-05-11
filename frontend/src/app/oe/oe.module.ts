import { NgModule } from '@angular/core';
import { OeMasterPageComponent } from './components/oe-master-page/oe-master-page.component';
import { PreviewComponent } from './components/preview/preview.component';
import { OeRoutingModule } from './oe.routing.modules';

@NgModule({
  declarations: [
    OeMasterPageComponent,
    PreviewComponent,
  ],
  imports: [
    OeRoutingModule,
  ],
  providers: [
    PreviewComponent,
  ],
  exports: [
  ]
})

export class OeEnergyModule {
}
