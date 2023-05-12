import { BaseBoxV2Component } from './components/base-box-v2/base-box-v2.component';
import { BlockspanComponent } from './components/blockspan/blockspan.component';
import { NgModule } from '@angular/core';
import { OeMasterPageComponent } from './components/oe-master-page/oe-master-page.component';
import { PreviewComponent } from './components/preview/preview.component';
import { OeRoutingModule } from './oe.routing.modules';
import { EnergyDetailComponent } from './components/energy-detail/energy-detail.component';
import { CommonModule } from '@angular/common';
import { OpEnergyApiService } from './services/oe-energy.service';
import { ToastrModule } from 'ngx-toastr';

@NgModule({
  declarations: [
    OeMasterPageComponent,
    PreviewComponent,
    BlockspanComponent,
    BaseBoxV2Component,
    EnergyDetailComponent,
  ],
  imports: [CommonModule, OeRoutingModule, ToastrModule.forRoot()],
  providers: [
    OpEnergyApiService,
    PreviewComponent,
    BlockspanComponent,
    BaseBoxV2Component,
    EnergyDetailComponent,
  ],
  exports: [ToastrModule],
})
export class OeEnergyModule {}
