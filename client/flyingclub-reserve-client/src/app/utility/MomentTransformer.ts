import { autoserialize, autoserializeAs, serialize, deserialize } from 'cerialize';
import * as moment from 'moment';

// source: https://github.com/weichx/cerialize/issues/55

export const MomentTransformer = {
  Serialize(value: any): string {
    return value.toISOString();
  },
  Deserialize(json: any): any {    
    return moment(json);
  }
}