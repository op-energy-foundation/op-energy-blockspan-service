import { ClipboardComponent } from './components/clipboard/clipboard.component';
import { BlockspanNavigatorComponent } from './components/blockspan-navigator/blockspan-navigator.component';
import { StrikeSummaryComponent } from './components/strike-summary/strike-summary.component';
import { BaseBoxHorComponent } from './components/base-box-hor/base-box-hor.component';
import { StrikeDetailComponent } from './components/strike-detail/strike-detail.component';
import { NgModule } from '@angular/core';
import { ToastrModule } from 'ngx-toastr';
import { CommonModule } from '@angular/common';
import { AngularSvgIconModule } from 'angular-svg-icon';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
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
import { OeEnergyApiService } from './services/oe-energy.service';
import { BlockspansHomeComponent } from './components/blockspans-home/blockspans-home.component';
import { WebsocketService } from './services/websocket.service';
import { OeStateService } from './services/state.service';
import {
  faBook,
  faCalendarAlt,
  faChartArea,
  faDownload,
  faInfoCircle,
} from '@fortawesome/free-solid-svg-icons';
import { OeDocsComponent } from './components/oe-docs/oe-docs.component';
import { AboutComponent } from './components/about/about.component';
import { NgbDatepickerModule, NgbDropdownModule, NgbTooltipModule } from '@ng-bootstrap/ng-bootstrap';
import { BlockRatesGraphComponent } from './components/blockrates-graph/block-rates-graph.component';
import { GraphsComponent } from './components/graphs/graphs.component';
import { NgxEchartsModule } from 'ngx-echarts';
import * as echarts from 'echarts';
import { LoginComponent } from './components/login/login.component';
import { StrikesRangeComponent } from './components/strikes-range/strikes-range.component';
import { DataTableComponent } from './components/data-table/data-table.component';
import { CloudsvgComponent } from './components/svg/cloudsvg/cloudsvg.component';
import { WatersvgComponent } from './components/svg/watersvg/watersvg.component';
import { IcesvgComponent } from './components/svg/icesvg/icesvg.component';
import { FiresvgComponent } from './components/svg/firesvg/firesvg.component';
import { GuessingBlockComponent } from './components/guessing-block/guessing-block.component';
import { StrikeSummaryWithGuessComponent } from './components/strike-summary-with-guess/strike-summary-with-guess.component';
import { GuessingGameComponent } from './components/guessing-game/guessing-game.component';
import { BoxComponent } from './components/box/box.component';
import { BlockspanBHSComponent } from './components/blockspan-bhs/blockspan-bhs.component';
import { MyGuessesComponent } from './components/my-guesses/my-guesses.component';
import { BlockRateStrikeDetailsV2Component } from './components/block-rate-strike-details-v2/block-rate-strike-details-v2.component';
import { BlockrateStrikeSummaryV2Component } from './components/blockrate-strike-summary-v2/blockrate-strike-summary-v2.component';

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
    OeDocsComponent,
    AboutComponent,
    GraphsComponent,
    BlockRatesGraphComponent,
    LoginComponent,
    ClipboardComponent,
    StrikesRangeComponent,
    DataTableComponent,
    GuessingBlockComponent,
    CloudsvgComponent,
    FiresvgComponent,
    IcesvgComponent,
    WatersvgComponent,
    StrikeSummaryWithGuessComponent,
    GuessingGameComponent,
    BlockspanBHSComponent,
    BoxComponent,
    MyGuessesComponent,
    BlockRateStrikeDetailsV2Component,
    BlockrateStrikeSummaryV2Component,
  ],
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    OeRoutingModule,
    ToastrModule.forRoot(),
    AngularSvgIconModule.forRoot(),
    FontAwesomeModule,
    NgbDropdownModule,
    NgbDatepickerModule,
    NgxEchartsModule.forRoot({ echarts }),
    NgbTooltipModule,
  ],
  providers: [
    WebsocketService,
    OeEnergyApiService,
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
    OeDocsComponent,
    AboutComponent,
    StrikesRangeComponent,
  ],
  exports: [ToastrModule, AngularSvgIconModule, FontAwesomeModule],
})
export class OeEnergyModule {
  constructor(library: FaIconLibrary) {
    library.addIcons(faBook);
    library.addIcons(faInfoCircle);
    library.addIcons(faChartArea);
    library.addIcons(faDownload);
    library.addIcons(faCalendarAlt);
  }
}
