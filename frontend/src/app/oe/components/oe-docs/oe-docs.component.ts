import { Component, Inject, OnInit, OnDestroy } from '@angular/core';
import { AfterViewInit } from '@angular/core';
import { OeEnergyApiService } from './../../services/oe-energy.service';
import { OeAccountApiService } from './../../services/oe-energy.service';
import { OeBlocktimeApiService, OeInternalBlocktimeApiService } from './../../services/oe-energy.service';
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
  swaggerDomBlockspanService: HTMLElement;
  swaggerDomAccountService: HTMLElement;
  swaggerDomBlocktimeService: HTMLElement;
  swaggerDomInternalBlocktimeService: HTMLElement;
  blockspanServiceSpecSubscription: Subscription;
  accountServiceSpecSubscription: Subscription;
  blocktimeServiceSpecSubscription: Subscription;
  internalBlocktimeServiceSpecSubscription: Subscription;

  constructor(
    @Inject(DOCUMENT) public document: Document,
    private oeEnergyApiService: OeEnergyApiService,
    private oeAccountApiService: OeAccountApiService,
    private oeBlocktimeApiService: OeBlocktimeApiService,
    private oeInternalBlocktimeApiService: OeInternalBlocktimeApiService,
  ) {}

  ngOnInit() {}

  ngOnDestroy() {
    this.blockspanServiceSpecSubscription.unsubscribe();
    this.accountServiceSpecSubscription.unsubscribe();
    this.blocktimeServiceSpecSubscription.unsubscribe();
  }

  ngAfterViewInit() {
    this.swaggerDomBlockspanService = this.document.getElementById('swagger-blockspan-service');
    this.swaggerDomAccountService = this.document.getElementById('swagger-account-service');
    this.swaggerDomBlocktimeService = this.document.getElementById('swagger-blocktime-service');
    this.swaggerDomInternalBlocktimeService = this.document.getElementById('swagger-internal-blocktime-service');
    this.blockspanServiceSpecSubscription = this.oeEnergyApiService
      .$getSwaggerFile()
      .subscribe((data) => {
        SwaggerUIBundle({
          spec: data,
          domNode: this.swaggerDomBlockspanService,
          deepLinking: true,
          presets: [SwaggerUIBundle.presets.apis, SwaggerUIStandalonePreset],
          layout: 'StandaloneLayout',
        });
      });
    this.accountServiceSpecSubscription = this.oeAccountApiService
      .$getSwaggerFile()
      .subscribe((data) => {
        SwaggerUIBundle({
          spec: data,
          domNode: this.swaggerDomAccountService,
          deepLinking: true,
          presets: [SwaggerUIBundle.presets.apis, SwaggerUIStandalonePreset],
          layout: 'StandaloneLayout',
        });
      });
    this.blocktimeServiceSpecSubscription = this.oeBlocktimeApiService
      .$getSwaggerFile()
      .subscribe((data) => {
        SwaggerUIBundle({
          spec: data,
          domNode: this.swaggerDomBlocktimeService,
          deepLinking: true,
          presets: [SwaggerUIBundle.presets.apis, SwaggerUIStandalonePreset],
          layout: 'StandaloneLayout',
        });
      });
    this.internalBlocktimeServiceSpecSubscription = this.oeInternalBlocktimeApiService
      .$getSwaggerFile()
      .subscribe((data) => {
        SwaggerUIBundle({
          spec: data,
          domNode: this.swaggerDomInternalBlocktimeService,
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
