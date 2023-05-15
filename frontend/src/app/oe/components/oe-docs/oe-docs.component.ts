import { Component, Inject, OnInit, OnDestroy } from '@angular/core';
import { AfterViewInit } from '@angular/core';
import { OeEnergyApiService } from './../../services/oe-energy.service';
import { Subscription } from 'rxjs';
import { DOCUMENT } from '@angular/common';
import { SwaggerUIBundle, SwaggerUIStandalonePreset } from 'swagger-ui-dist';

@Component({
  selector: 'app-oe-docs',
  templateUrl: './oe-docs.component.html',
  styleUrls: ['./oe-docs.component.scss'],
})
export class OeDocsComponent implements OnDestroy, OnInit, AfterViewInit {
  navCollapsed = false;
  isMobile = window.innerWidth <= 767.98;
  swaggerDom: HTMLElement;
  spec = {};
  specSubscription: Subscription;

  constructor(
    @Inject(DOCUMENT) public document: Document,
    private oeEnergyApiService: OeEnergyApiService
  ) {}

  ngOnInit() {}

  ngOnDestroy() {
    this.specSubscription.unsubscribe();
  }

  ngAfterViewInit() {
    this.swaggerDom = this.document.getElementById('swagger');
    this.specSubscription = this.oeEnergyApiService
      .$getSwaggerFile()
      .subscribe((data) => {
        SwaggerUIBundle({
          spec: data,
          domNode: this.swaggerDom,
          deepLinking: true,
          presets: [SwaggerUIBundle.presets.apis, SwaggerUIStandalonePreset],
          layout: 'StandaloneLayout',
        });
      });
  }

  collapse(): void {
    this.navCollapsed = !this.navCollapsed;
  }

  onResize(event: any) {
    this.isMobile = window.innerWidth <= 767.98;
  }
}
