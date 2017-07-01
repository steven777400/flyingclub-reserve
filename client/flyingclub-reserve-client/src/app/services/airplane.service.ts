import { Injectable } from '@angular/core';
import { Headers, Http } from '@angular/http';
import { Session } from 'app/models/session';
import { Deserialize } from 'cerialize';
import { environment } from 'environments/environment';

import { Airplane } from 'app/models/Airplane';
import { AuthenticationService } from 'app/services/authentication.service';


@Injectable()
export class AirplaneService {

  constructor(private http: Http, private authentication: AuthenticationService) { }

  public async airplanes() : Promise<Airplane[]> {

    const planes = await this.http
      .get(`${environment.serviceUrl}/airplanes`, {headers: this.authentication.getHeaders()})
      .toPromise();
      
    return Deserialize(planes.json(), Airplane);      
  }

}
