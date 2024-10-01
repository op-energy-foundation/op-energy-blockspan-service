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
            path: 'energy_detail/:from/:to',
            component: EnergyDetailComponent,
          },
          {
            path: 'energy_summary',
            component: EnergySummaryComponent,
          },
          {
            path: 'strike_detail',
            component: StrikeDetailComponent,
          },
          {
            path: 'strike_summary',
            component: StrikeSummaryComponent,
          },
          {
            path: 'strike_summary_with_guess',
            component: StrikeSummaryWithGuessComponent,
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
            path: 'strikes-range',
            component: StrikesRangeComponent,
          },
          {
            path: 'blockspan-details',
            component: BlockspanBHSComponent,
          },
          {
            path: 'my_guesses',
            component: MyGuessesComponent
          },
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
