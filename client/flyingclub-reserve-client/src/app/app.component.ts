import { Component } from '@angular/core';
import { Headers, Http } from '@angular/http';
import { Serialize, Deserialize } from 'cerialize';
import { Moment } from  'moment';
import * as moment from 'moment';

import { Session } from 'app/models/Session';
import { Reservation } from 'app/models/Reservation';
import { Airplane } from 'app/models/Airplane';
import { AuthenticationService } from 'app/services/authentication.service';

class Login {
  username: string;
  password: string;

}

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  title = 'app';
  model = new Login();
  
  constructor(private http: Http, private authentication: AuthenticationService) { }

  login() {        
    
    console.log(this.model);
  
    

    this.authentication
      .login(this.model.username, this.model.password)
      .then(s => {
        console.log(s);
        this.authentication.activate()
        .then(s2 => {
          console.log(s2);
          this.http
            .get("http://localhost:8080/airplanes", {headers: this.authentication.getHeaders()})
            .toPromise()
            .then(r2 => {
              console.log(r2);     
              const r2a:Airplane[] = Deserialize(r2.json(), Airplane);
              console.log(r2a);
              const o = new Reservation();
              o.reservationAirplaneId = r2a[0].id;
              o.reservationComment = "comment";
              o.reservationMaintenance = false;
              o.reservationUserId = s.sessionUserId;
              o.reservationStart = moment([2017, 7, 14, 15, 25, 50]);
              o.reservationEnd = moment([2017, 8, 14, 15, 25, 50]);
              console.log(o);
              console.log(Serialize(o));
              this.http
                .post("http://localhost:8080/reservation", Serialize(o), {headers: this.authentication.getHeaders()})
                .toPromise()
                .then(r3 => { console.log(r3); });

              
            });
        });
        
      });
      // https://medium.com/@amcdnl/global-error-handling-with-angular2-6b992bdfb59c


      


  }
}
