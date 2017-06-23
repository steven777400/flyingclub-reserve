import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import {NgbModule} from '@ng-bootstrap/ng-bootstrap';
import { FormsModule } from '@angular/forms';
import { HttpModule }    from '@angular/http';

import { AuthenticationService } from 'app/services/authentication.service';

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
  providers: [AuthenticationService],
  bootstrap: [AppComponent]
})
export class AppModule { }
