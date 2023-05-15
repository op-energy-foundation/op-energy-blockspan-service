import { BlockspanNavigatorComponent } from './components/blockspan-navigator/blockspan-navigator.component';
import { StrikeSummaryComponent } from './components/strike-summary/strike-summary.component';
import { BaseBoxHorComponent } from './components/base-box-hor/base-box-hor.component';
import { StrikeDetailComponent } from './components/strike-detail/strike-detail.component';
import { NgModule } from '@angular/core';
import { ToastrModule } from 'ngx-toastr';
import { CommonModule } from '@angular/common';
import { AngularSvgIconModule } from 'angular-svg-icon';
import { FormsModule } from '@angular/forms';
import { EnergyComponent } from './components/energy/energy.component';
import { BaseBoxComponent } from './components/base-box/base-box.component';
import { StrikeComponent } from './components/strike/strike.component';
import { EnergySummaryComponent } from './components/energy-summary/energy-summary.component';
import { BaseBoxV2Component } from './components/base-box-v2/base-box-v2.component';
import { BlockspanComponent } from './components/blockspan/blockspan.component';
import {
  FontAwesomeModule,
  FaIconLibrary,
} from '@fortawesome/angular-fontawesome';
import { OeMasterPageComponent } from './components/oe-master-page/oe-master-page.component';
import { PreviewComponent } from './components/preview/preview.component';
import { OeRoutingModule } from './oe.routing.modules';
import { EnergyDetailComponent } from './components/energy-detail/energy-detail.component';
import { OpEnergyApiService } from './services/oe-energy.service';
import { BlockspansHomeComponent } from './components/blockspans-home/blockspans-home.component';
import { WebsocketService } from './services/websocket.service';
import { OeStateService } from './services/state.service';

@NgModule({
  declarations: [
    OeMasterPageComponent,
    PreviewComponent,
    BlockspanComponent,
    BaseBoxV2Component,
    EnergyDetailComponent,
    EnergySummaryComponent,
    StrikeComponent,
    BaseBoxComponent,
    EnergyComponent,
    StrikeDetailComponent,
    BaseBoxHorComponent,
    StrikeSummaryComponent,
    BlockspanNavigatorComponent,
    BlockspansHomeComponent,
  ],
  imports: [
    CommonModule,
    FormsModule,
    OeRoutingModule,
    ToastrModule.forRoot(),
    AngularSvgIconModule.forRoot(),
    FontAwesomeModule,
  ],
  providers: [
    WebsocketService,
    OpEnergyApiService,
    OeStateService,
    PreviewComponent,
    BlockspanComponent,
    BaseBoxV2Component,
    EnergyDetailComponent,
    EnergySummaryComponent,
    StrikeComponent,
    BaseBoxComponent,
    EnergyComponent,
    StrikeDetailComponent,
    BaseBoxHorComponent,
    StrikeSummaryComponent,
    BlockspanNavigatorComponent,
    BlockspansHomeComponent,
  ],
  exports: [ToastrModule, FontAwesomeModule],
})
export class OeEnergyModule {
  constructor(library: FaIconLibrary) {}
}
