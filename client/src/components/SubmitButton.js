import React from 'react';
import Button from '@material-ui/core/Button';
import CircularProgress from '@material-ui/core/CircularProgress';
import { withStyles } from '@material-ui/core/styles';

const styles = theme => ({  
  
});


class SubmitButton extends React.Component {
  constructor(props) {
    super(props);    
    
    this.state = {disabled: false};

    this.handleSubmit = this.handleSubmit.bind(this);

    this.props.mountSubmit(this.handleSubmit);
  }

  handleSubmit(promise) {
    
    this.setState({disabled: true});

    promise
      .then(_ => this.setState({disabled: false}));    
    
  }  
  

  render() {
    const { classes } = this.props;

    return this.state.disabled ? 
      (<Button type="submit" variant="contained" color="secondary" disabled>
        <CircularProgress size={24} />
        {this.props.label}
        </Button>)
      :
      (<Button type="submit" variant="contained" color="primary" >{this.props.icon} {this.props.label}</Button>)
    ;
    
    }
  
  }
  
  export default withStyles(styles)(SubmitButton);