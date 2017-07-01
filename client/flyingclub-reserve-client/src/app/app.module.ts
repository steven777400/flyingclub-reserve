import { BrowserModule } from '@angular/platform-browser';
import { NgModule, ErrorHandler } from '@angular/core';
import { NgbModule } from '@ng-bootstrap/ng-bootstrap';
import { FormsModule } from '@angular/forms';
import { HttpModule }    from '@angular/http';

import { GlobalErrorHandler } from './utility/error-handler';

import { AuthenticationService } from 'app/services/authentication.service';
import { AirplaneService } from 'app/services/airplane.service';
import { ReservationService } from 'app/services/reservation.service';

import { AppComponent } from './app.component';

import 'rxjs/add/operator/toPromise';

@NgModule({
  declarations: [
    AppComponent
  ],
  imports: [
    NgbModule.forRoot(),
    FormsModule,
    HttpModule,
    BrowserModule
  ],
  providers: [AuthenticationService, AirplaneService, ReservationService,
    {
      provide: ErrorHandler, 
      useClass: GlobalErrorHandler
    }],
  bootstrap: [AppComponent]
})
export class AppModule { }
