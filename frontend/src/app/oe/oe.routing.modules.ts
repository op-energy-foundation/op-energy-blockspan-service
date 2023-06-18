import { StrikeSummaryWithGuessComponent } from './components/strike-summary-with-guess/strike-summary-with-guess.component';
import { GraphsComponent } from './components/graphs/graphs.component';
import { AboutComponent } from './components/about/about.component';
import { OeDocsComponent } from './components/oe-docs/oe-docs.component';
import { BlockspansHomeComponent } from './components/blockspans-home/blockspans-home.component';
import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { EnergyDetailComponent } from './components/energy-detail/energy-detail.component';
import { PreviewComponent } from './components/preview/preview.component';
import { OeMasterPageComponent } from './components/oe-master-page/oe-master-page.component';
import { EnergySummaryComponent } from './components/energy-summary/energy-summary.component';
import { StrikeDetailComponent } from './components/strike-detail/strike-detail.component';
import { StrikeSummaryComponent } from './components/strike-summary/strike-summary.component';
import { BlockRatesGraphComponent } from './components/blockrates-graph/block-rates-graph.component';

const routes: Routes = [
  { path: '', redirectTo: '/preview-page', pathMatch: 'full' },
  {
    path: '',
    component: OeMasterPageComponent,
    children: [
      {
        path: 'preview-page',
        component: PreviewComponent,
      },
      {
        path: 'docs',
        component: OeDocsComponent,
      },
      {
        path: 'about',
        component: AboutComponent,
      },
      {
        path: 'strikes/nbdr/next/:blockSpan',
        component: StrikeSummaryWithGuessComponent,
      },
      {
        path: 'hashstrikes',
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
          {
            path: 'strike_summary/:from/:to',
            component: StrikeSummaryComponent,
          },
          {
            path: 'blockspans/:span',
            component: BlockspansHomeComponent,
          },
          {
            path: 'blockspans/:span/:tip',
            component: BlockspansHomeComponent,
          }
        ],
      },
      {
        path: 'graphs',
        component: GraphsComponent,
        children: [
          {
            path: '',
            redirectTo: 'blockratecharts/144',
            pathMatch: 'full',
          },
          {
            path: 'blockratecharts/:span',
            component: BlockRatesGraphComponent,
          },
        ],
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
