import { AuthInterceptor } from './oe/interceptors/auth.interceptor';
import { CookieService } from 'ngx-cookie-service';
import { HTTP_INTERCEPTORS, HttpClientModule } from '@angular/common/http';
import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { OeEnergyApiService } from './oe/services/oe-energy.service';
import { OeEnergyModule } from './oe/oe.module';
import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { WebsocketService } from './oe/services/websocket.service';
import { ClipboardModule } from 'ngx-clipboard';

@NgModule({
  declarations: [AppComponent],
  imports: [
    BrowserModule,
    HttpClientModule,
    BrowserAnimationsModule,
    OeEnergyModule,
    AppRoutingModule,
    ClipboardModule,
  ],
  providers: [
    CookieService,
    {
      provide: HTTP_INTERCEPTORS,
      useClass: AuthInterceptor,
      multi: true,
    },
    WebsocketService,
    OeEnergyApiService,
  ],
  bootstrap: [AppComponent],
})
export class AppModule {}
