import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { EnergyDetailComponent } from './components/energy-detail/energy-detail.component';
import { PreviewComponent } from './components/preview/preview.component';
import { OeMasterPageComponent } from './components/oe-master-page/oe-master-page.component';
import { EnergySummaryComponent } from './components/energy-summary/energy-summary.component';
import { StrikeDetailComponent } from './components/strike-detail/strike-detail.component';

const routes: Routes = [
  {
    path: 'hashstrikes',
    component: OeMasterPageComponent,
    children: [
      {
        path: 'energy_detail/:from/:to',
        component: EnergyDetailComponent,
      },
      {
        path: 'energy_summary/:from/:to',
        component: EnergySummaryComponent,
      },
      {
        path: 'strike_detail/:from/:to/:strikeBlockHeight/:strikeMedianTime/:strikeCreationTime',
        component: StrikeDetailComponent,
      },
    ],
  },
  {
    path: '',
    component: OeMasterPageComponent,
    children: [
      {
        path: 'preview-page',
        component: PreviewComponent,
      },
    ],
  },
  {
    path: '**',
    redirectTo: 'preview-page',
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class OeRoutingModule {}
