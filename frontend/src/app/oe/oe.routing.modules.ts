import { EnergyDetailComponent } from './components/energy-detail/energy-detail.component';
import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { PreviewComponent } from './components/preview/preview.component';
import { OeMasterPageComponent } from './components/oe-master-page/oe-master-page.component';

const routes: Routes = [
  {
    path: 'hashstrikes',
    component: OeMasterPageComponent,
    children: [
      {
        path: 'energy_detail/:from/:to',
        component: EnergyDetailComponent,
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
