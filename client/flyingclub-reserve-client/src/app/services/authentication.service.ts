import { Injectable } from '@angular/core';
import { Headers, Http } from '@angular/http';
import { Session } from 'app/models/session';
import { Deserialize } from 'cerialize';
import { environment } from 'environments/environment';

@Injectable()
export class AuthenticationService {

  public session: Session;

  constructor(private http: Http) { }

  public login(username: string, password: string) : Promise<Session> {

    const headers = new Headers({"Authorization": "Basic " + btoa(username + ":" + password)});

    return this.http
    .post(environment.serviceUrl + "/login", {}, {headers: headers})
    .toPromise()
    .then(r => {
      const s: Session = Deserialize(r.json(), Session);      
      this.session = s;
      return s;
    });

  }

  public activate() : Promise<Session> {

    const headers = new Headers({"Authorization": "Bearer " + this.session.id});

    return this.http
    .post(environment.serviceUrl + "/login", {}, {headers: headers})
    .toPromise()
    .then(r => {
      const s: Session = Deserialize(r.json(), Session);      
      this.session = s;
      return s;
    });

  }

  public getHeaders() : Headers {
    return new Headers({"Authorization": "Bearer " + this.session.id, "Auth-Key": this.session.sessionAuthKey});

  }

}
