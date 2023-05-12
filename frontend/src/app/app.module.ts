import { HttpClientModule } from '@angular/common/http';
import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { OpEnergyApiService } from './oe/services/oe-energy.service';
import { OeEnergyModule } from './oe/oe.module';
import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';

@NgModule({
  declarations: [AppComponent],
  imports: [
    BrowserModule,
    AppRoutingModule,
    HttpClientModule,
    BrowserAnimationsModule,
    OeEnergyModule,
  ],
  providers: [OpEnergyApiService],
  bootstrap: [AppComponent],
})
export class AppModule {}
