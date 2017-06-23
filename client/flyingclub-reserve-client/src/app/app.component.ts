import { Component } from '@angular/core';
import { Headers, Http } from '@angular/http';

import { Session } from 'app/models/Session';
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
            .get("http://localhost:8080/users", {headers: this.authentication.getHeaders()})
            .toPromise()
            .then(r2 => {
              console.log(r2)
            });
        });
        
      });
      // https://medium.com/@amcdnl/global-error-handling-with-angular2-6b992bdfb59c


      


  }
}
