import { autoserialize, autoserializeAs } from 'cerialize';
import {MomentTransformer} from 'app/utility/MomentTransformer';
import {Moment} from  'moment';

export class Session {

  @autoserialize public id: string;
  @autoserialize public sessionAuthKey: string;
  @autoserializeAs(MomentTransformer) public sessionCreated: Moment;
  @autoserializeAs(MomentTransformer) public sessionExpired: Moment;
  @autoserializeAs(MomentTransformer) public sessionUsed: Moment;
  @autoserialize public sessionUserId: string;  

  
}
