import { autoserialize, autoserializeAs } from 'cerialize';
import { MomentTransformer } from 'app/utility/moment-transformer';
import { Moment } from  'moment';

export class Reservation {

  @autoserialize public id: string;
  @autoserialize public reservationUserId: string;
  @autoserialize public reservationAirplaneId: string;
  @autoserializeAs(MomentTransformer) public reservationStart: Moment;
  @autoserializeAs(MomentTransformer) public reservationEnd: Moment;
  @autoserialize public reservationMaintenance: boolean;
  @autoserialize public reservationComment: string;  

  
}
