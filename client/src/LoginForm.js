import React from 'react';

class LoginForm extends React.Component {
  constructor(props) {
    super(props);
    this.state = {userid: '', pin: ''};

    this.handleChange = this.handleChange.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
  }

  handleChange(event) {
    const target = event.target;
    
    this.setState({
      [target.name]: target.value
    });
  }

  handleSubmit(event) {
    event.preventDefault();

    console.log(this.state);

    fetch('http://localhost:8080/login', {
      method: "POST",
      headers: {        
        "Authorization": 'Basic ' + btoa(this.state.userid + ":" + this.state.pin)
      },      
    })
      .then(results => {
        console.log(results);
      });      
    
  }

  render() {
    return (      
      <form onSubmit={this.handleSubmit}>                  
          <input type="text" name="userid" placeholder="email or phone #" value={this.state.userid} onChange={this.handleChange} />    
          <br />
          <input type="password" name="pin" placeholder="PIN" value={this.state.pin} onChange={this.handleChange} />    
          <br />          
          <input type="submit" value="Submit" />
      </form>
    );
  }

}

export default LoginForm;