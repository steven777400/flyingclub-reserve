import React, { Component } from 'react';
import CssBaseline from '@material-ui/core/CssBaseline';
import Grid from '@material-ui/core/Grid';
import { withStyles } from '@material-ui/core/styles';

import MUIDataTable from 'mui-datatables';

import './App.css';
import LoginForm from './LoginForm.js';
import Announcement from './components/Announcement.js';

const styles = theme => ({  
  
});

const columns = [
  {name: "userLastname", label: "Last name"},
  {name: "userFirstname", label: "First name"},
  {name: "userPermission", label: "Permission"},
];

const options = {
  filterType: 'checkbox',
};


class App extends Component {
  constructor(props) {
    super(props);

    this.state = { data: null };

    this.lgOnLogin = this.lgOnLogin.bind(this);
  }

  lgOnLogin(x) {
    console.log("onLogin");
    console.log(x);

    fetch('http://localhost:8080/users', {
      method: "GET",
      headers: {
        "Authorization": 'Bearer ' + x.id,
        "Auth-Key": x.sessionAuthKey
      }
    })
      .catch(_ => {return {ok: false};})      
      .then(results => {        
        if (results.ok) {
          results.json().then(data => {            
            data = data.map(row => {
              let r = row.user;
              r.phones = row.phones;
              r.emails = row.emails;              
              return r;
            });
            console.log(data);
            this.setState({data: data})
          });
        }
      });
  }
  render() {    

    return (
      <div>       
        <CssBaseline /> 
       
       <Grid container justify="center">
        <Grid item xs={10} md={4}>
          <Grid container>
          
            
            <Grid item xs={12}>
              <Announcement />
            </Grid>
            
            <Grid item xs={12}>
              <LoginForm onLogin={this.lgOnLogin} />        
            </Grid>

        </Grid>
      </Grid>
      </Grid>


      {this.state.data ?
      (<MUIDataTable
        title={"Employee List"}
        data={this.state.data}
        columns={columns}
        options={options}
      />) : ''}

        
      </div>
    );
  }
}


export default withStyles(styles)(App);
