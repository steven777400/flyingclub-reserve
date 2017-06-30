import { autoserialize, autoserializeAs } from 'cerialize';
import { MomentTransformer } from 'app/utility/moment-transformer';
import { Moment } from  'moment';

export class Airplane {

  @autoserialize public id: string;
  @autoserialize public airplaneTail: string;
  @autoserialize public airplaneDescription: string;  
  @autoserializeAs(MomentTransformer) public airplaneDeleted: Moment;  
  
}

