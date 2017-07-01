import { Injectable } from '@angular/core';
import { Headers, Http } from '@angular/http';
import { Session } from 'app/models/session';
import { Deserialize, Serialize } from 'cerialize';
import { environment } from 'environments/environment';

import { Reservation } from 'app/models/Reservation';
import { AuthenticationService } from 'app/services/authentication.service';


@Injectable()
export class ReservationService {

  constructor(private http: Http, private authentication: AuthenticationService) { }

  public async reserve(reservation : Reservation) : Promise<string> {

    const r = await this.http
      .post(`${environment.serviceUrl}/reservation`, Serialize(reservation), {headers: this.authentication.getHeaders()})
      .toPromise();
    
    return r.text();
        
  }

}
