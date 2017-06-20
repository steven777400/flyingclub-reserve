import { Component } from '@angular/core';
import { Headers, Http } from '@angular/http';

import 'rxjs/add/operator/toPromise';

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
    .then(r => console.log(r.json()))
    .catch(r => console.log(r));

  }
}
