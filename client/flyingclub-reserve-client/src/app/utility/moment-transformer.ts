import { autoserialize, autoserializeAs, serialize, deserialize } from 'cerialize';
import * as moment from 'moment';
import { Moment } from  'moment';

// source: https://github.com/weichx/cerialize/issues/55

export const MomentTransformer = {
  Serialize(value: Moment): string {
    if (value.creationData().isUTC)
      return value.toISOString(); // Use Z to indicate UTC
    else
      return value.format("YYYY-MM-DDTHH:mm:ss"); // Local time, WITHOUT timezone component
  },
  Deserialize(json: any): Moment {    
    return moment(json);
  }
}