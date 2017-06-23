import { Component } from '@angular/core';
import { Headers, Http } from '@angular/http';

import 'rxjs/add/operator/toPromise';

import { Deserialize } from 'cerialize';
import {Session} from 'app/models/Session';

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
  
  constructor(private http: Http) { }

  login() {        
    
    console.log(this.model);
  
    const headers = new Headers({"Authorization": "Basic " + btoa(this.model.username + ":" + this.model.password)});

    this.http
    .post("http://localhost:8080/login", {}, {headers: headers})
    .toPromise()
    .then(r => {
      const s: Session = Deserialize(r.json(), Session);
      console.log(s);
      console.log(s.sessionCreated);
      console.log(s.sessionCreated.format());

      const headers2 = new Headers({"Authorization": "Bearer " + s.id, "Auth-Key": s.sessionAuthKey});

      this.http
      .get("http://localhost:8080/users", {headers: headers2})
      .toPromise()
      .then(r2 => console.log(r2));


    })
    .catch(r => console.log(r));

  }
}
