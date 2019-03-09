import React from 'react';
import TextField from '@material-ui/core/TextField';
import Paper from '@material-ui/core/Paper';
import Typography from '@material-ui/core/Typography';
import ErrorIcon from '@material-ui/icons/Error';
import SendIcon from '@material-ui/icons/Send';
import { withStyles } from '@material-ui/core/styles';

import SubmitButton from './components/SubmitButton.js';

const styles = theme => ({
  paper: {
    padding: theme.spacing.unit * 2,
  },
  textField: {
    width: '100%',
  },
  submit: {
    marginTop: '5px'
  }
});

class LoginForm extends React.Component {
  constructor(props) {
    super(props);
    this.state = { userid: '', pin: '', showError: false, submitFunction: null };

    this.handleChange = this.handleChange.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
    this.handleFormSubmit = this.handleFormSubmit.bind(this);
    this.mountSubmit = this.mountSubmit.bind(this);
    
  }

  handleChange(event) {
    const target = event.target;

    this.setState({
      [target.name]: target.value
    });
  }

  handleSubmit() {
        
    this.setState({ showError: false });

    return fetch('http://localhost:8080/login', {
      method: "POST",
      headers: {
        "Authorization": 'Basic ' + btoa(this.state.userid + ":" + this.state.pin)
      }
    })
      .catch(_ => {return {ok: false};})
      .then(results => {        
        // if it's good
        if (results.ok) {
          results.json().then(x => this.props.onLogin(x));
        } else {          
          this.setState({ pin: '', showError: true });

        }

      })
      
      ;

  }

  mountSubmit(f) {
    this.setState({submitFunction: f});
  }

  handleFormSubmit(event) {
    event.preventDefault();    
    this.state.submitFunction(this.handleSubmit());

  }

  render() {
    const { classes } = this.props;

    return (

      <Paper className={classes.paper}>
        <form onSubmit={this.handleFormSubmit}>
          <Typography component="h1" variant="h5">
            Sign in
          </Typography>

          <div><TextField className={classes.textField} name="userid" value={this.state.userid} onChange={this.handleChange} label="email or phone #" required /></div>
          <div><TextField className={classes.textField} type="password" name="pin" value={this.state.pin} onChange={this.handleChange} label="PIN" required /></div>
          <div className={classes.submit}><SubmitButton label="Login" icon={<SendIcon />} mountSubmit={this.mountSubmit}/></div>

          {this.state.showError ?
            (<div style={{ display: 'flex', alignItems: 'center' }}>
              
              <ErrorIcon color="error" />
              <Typography inline={true} color="error">Unable to log you in; check id and PIN</Typography>
            
            </div>)
            : ''}


        </form>
      </Paper>

    );
  }

}

export default withStyles(styles)(LoginForm);