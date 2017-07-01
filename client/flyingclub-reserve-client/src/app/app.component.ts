import { Component } from '@angular/core';
import { Headers, Http } from '@angular/http';
import { Serialize, Deserialize } from 'cerialize';
import { Moment } from  'moment';
import * as moment from 'moment';

import { Session } from 'app/models/Session';
import { Reservation } from 'app/models/Reservation';
import { Airplane } from 'app/models/Airplane';

import { AuthenticationService } from 'app/services/authentication.service';
import { AirplaneService } from 'app/services/airplane.service';
import { ReservationService } from 'app/services/reservation.service';

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
  
  constructor(private http: Http, 
    private authentication: AuthenticationService,
    private airplane: AirplaneService,
    private reserve: ReservationService) { }

  async login() {        
    
    console.log(this.model);
  
    

    const s = await this.authentication
      .login(this.model.username, this.model.password);

  
    console.log(s);
    const s2 = await this.authentication.activate();
    
    console.log(s2);
    const r2a = await this.airplane.airplanes();
              
    console.log(r2a);
    const o = new Reservation();
    o.reservationAirplaneId = r2a[0].id;
    o.reservationComment = "comment";
    o.reservationMaintenance = false;
    o.reservationUserId = s.sessionUserId;
    o.reservationStart = moment.utc([2017, 7, 14, 15, 25, 50]);
    o.reservationEnd = moment([2017, 8, 14, 15, 25, 50]);
    console.log(o);
    console.log(Serialize(o));

    const r3 = await this.reserve.reserve(o);
    console.log(r3); 

    
  
  
    
  
      // https://medium.com/@amcdnl/global-error-handling-with-angular2-6b992bdfb59c


      


  }
}
