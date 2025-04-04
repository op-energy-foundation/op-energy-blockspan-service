import { BlockrateSummaryV2Component } from './components/blockrate-summary-v2/blockrate-summary-v2.component';
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
import { LoginComponent } from './components/login/login.component';
import { StrikesRangeComponent } from './components/strikes-range/strikes-range.component';
import { BlockspanBHSComponent } from './components/blockspan-bhs/blockspan-bhs.component';
import { MyGuessesComponent } from './components/my-guesses/my-guesses.component';
import { BlockRateStrikeDetailsV2Component } from './components/block-rate-strike-details-v2/block-rate-strike-details-v2.component';
import { BlockrateStrikeSummaryV2Component } from './components/blockrate-strike-summary-v2/blockrate-strike-summary-v2.component';
import { HashrateComponent } from './components/hashrate/hashrate-details/hashrate.component';
import { HashrateSummaryComponent } from './components/hashrate/hashrate-summary/hashrate-summary.component';

const routes: Routes = [
  { path: 'login/:secret', component: LoginComponent },
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
        path: 'hashstrikes',
        children: [
          {
            path: 'blockrate-detail/:from/:to',
            component: EnergyDetailComponent,
          },
          {
            path: 'blockspans',
            component: BlockspansHomeComponent,
          },
          {
            path: 'blockspans/:from/:to',
            component: BlockspansHomeComponent,
          },
          {
            path: 'blockrate-strikes-range',
            component: StrikesRangeComponent,
          },
          {
            path: 'blockspan-details',
            component: BlockspanBHSComponent,
          },
          {
            path: 'my-guesses',
            component: MyGuessesComponent
          },
          {
            path: 'blockrate-strike-details',
            component: BlockRateStrikeDetailsV2Component
          },
          {
            path: 'blockrate-strike-summary',
            component: BlockrateStrikeSummaryV2Component
          },
          {
            path: 'blockrate-summary',
            component: BlockrateSummaryV2Component
          },
          {
            path: 'hashrate',
            component: HashrateComponent
          },
          {
            path: 'hashrate-summary',
            component: HashrateSummaryComponent
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
