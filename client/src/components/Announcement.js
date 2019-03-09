import React from 'react';
import Card from '@material-ui/core/Card';
import CardActionArea from '@material-ui/core/CardActionArea';
import CardActions from '@material-ui/core/CardActions';
import CardContent from '@material-ui/core/CardContent';
import { CardMedia } from '@material-ui/core';
import Button from '@material-ui/core/Button';
import Typography from '@material-ui/core/Typography';
import FlagIcon from '@material-ui/icons/Flag';
import { withStyles } from '@material-ui/core/styles';


const styles = theme => ({
  card: {
    display: 'flex',
    alignItems: 'center',   
    marginBottom: '5px' 
  },
  details: {
    display: 'flex',
    flexDirection: 'column',
  },
  content: {
    flex: '1 0 auto',
  },
  icon: {
    width: 60,
    height: 60,

  }
});

class Announcement extends React.Component {
  constructor(props) {
    super(props);
  }



  render() {
    const { classes } = this.props;

    return (

      <Card className={classes.card}>
        <div className={classes.details}>
          <CardContent>
            <FlagIcon className={classes.icon} color="secondary"/>
          </CardContent>
        </div>
        <div className={classes.details}>
        <CardContent>
          <Typography gutterBottom variant="h5" component="h2">
            System Announcement
                  </Typography>
          <Typography color="textSecondary">
            on 12:34 by Goat guy
                  </Typography>
          <Typography >
            Goats are the best animal.
                  </Typography>
        </CardContent>

        <CardActions>
          <Button size="small" color="primary">
            Acknowledge
                </Button>
        </CardActions>
        </div>
      </Card>


    );
  }

}

export default withStyles(styles)(Announcement);